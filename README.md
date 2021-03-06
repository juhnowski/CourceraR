# Печать головы списка
```
head(methods(print), 10)
```

# Конкатенация
```
paste("1","2")
[1] "1 2"
```
```
> paste0("qqq", "@jhu.edu")
[1] "qqq@jhu.edu"
```
# if-else
```
if(<condition>) {
      ## do something
}
      ## Continue with rest of code
```
```
if(<condition>) {
      ## do something
}
else {
      ## do something else
}
```
```
if(<condition1>) {
      ## do something
} else if(<condition2>) {
      ## do something different
} else {
      ## do something different
}
```
```
## Generate a uniform random number
x <- runif(1, 0, 10)
if(x > 3) {
        y <- 10
} else {
        y <- 0
}
```
# for Loops
Нормальное распределение
```
numbers <- rnorm(10)
for(i in 1:10) {
        print(numbers[i])
}
```
Этот цикл принимает переменную i и на каждой итерации цикла присваивает ей значения 1, 2, 3, ..., 10, выполняет код в фигурных скобках, а затем цикл завершается.
```
x <- c("a", "b", "c", "d")

for(i in 1:4) {
        ## Print out each element of 'x'
        print(x[i])  
}
```
Функция seq_along() обычно используется в сочетании с циклами for для создания целочисленной последовательности на основе длины объекта (в данном случае объекта x).
```
for(i in seq_along(x)) {   
        print(x[i])
}
```
```
for(letter in x) {
        print(letter)
}
```
```
for(i in 1:4) print(x[i])
```
# Вложенные циклы
```
> x <- matrix(1:6, 2, 3)
> x
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
```
```
for(i in seq_len(nrow(x))) {
        for(j in seq_len(ncol(x))) {
                print(x[i, j])
        }   
}
```
# next, break
```
for(i in 1:100) {
        if(i <= 20) {
                ## Skip the first 20 iterations
                next                 
        }
        ## Do something here
}
```
break используется для немедленного выхода из цикла, независимо от того, на какой итерации он может быть.
```
for(i in 1:100) {
      print(i)

      if(i > 20) {
              ## Stop loop after 20 iterations
              break  
      }     
}
```
# Загрузка, распаковка, фильтрация
```
library(readr)
library(dplyr)

## Download data from RStudio (if we haven't already)
if(!file.exists("data/2016-07-20.csv.gz")) {
        download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz", 
                      "data/2016-07-20.csv.gz")
}
cran <- read_csv("data/2016-07-20.csv.gz", col_types = "ccicccccci")
cran %>% filter(package == "filehash") %>% nrow
```
# Функция
```
library(dplyr)
library(readr)

## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date) {
        ## Construct web URL
        year <- substr(date, 1, 4)
        src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                       year, date)
        print("-------------------------------")
        print(date)
        print(year)
        print(src)
        print("-------------------------------")
        ## Construct path for storing local file
        dest <- file.path("data", basename(src))

        ## Don't download if the file is already there!
        if(!file.exists(dest))
                download.file(src, dest, quiet = TRUE)

        cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
        cran %>% filter(package == pkgname) %>% nrow
}
```
```
date <- "2016-07-20"
packagename <- "filehash"
num_download(packagename, date)
num_download("Rcpp", "2016-07-19")
```
# Значения аргументов функции по умолчанию
```
num_download <- function(pkgname, date = "2016-07-20") {
        year <- substr(date, 1, 4)
        src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                       year, date)
        dest <- file.path("data", basename(src))
        if(!file.exists(dest))
                download.file(src, dest, quiet = TRUE)
        cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
        cran %>% filter(package == pkgname) %>% nrow
}
```
```
num_download("Rcpp")
```
# Работа с ФС, принудительная остановка
```
check_for_logfile <- function(date) {
        year <- substr(date, 1, 4)
        src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                       year, date)
        dest <- file.path("data", basename(src))
        if(!file.exists(dest)) {
                val <- download.file(src, dest, quiet = TRUE)
                if(!val)
                        stop("unable to download file ", src)
        }
        dest
}

num_download <- function(pkgname, date = "2016-07-20") {
        dest <- check_for_logfile(date)
        cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
        cran %>% filter(package == pkgname) %>% nrow
}    
```

# Проверка зависимостей
```
check_pkg_deps <- function() {
        if(!require(readr)) {
                message("installing the 'readr' package")
                install.packages("readr")
        }
        if(!require(dplyr))
                stop("the 'dplyr' package needs to be installed first")
}

num_download <- function(pkgname, date = "2016-07-20") {
        check_pkg_deps()
        dest <- check_for_logfile(date)
        cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
        cran %>% filter(package == pkgname) %>% nrow
}
```

# Векторизация
в R общепринятой парадигмой является то, что функции принимают векторные аргументы и возвращают результаты в виде векторов или списков
```
## 'pkgname' can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
        check_pkg_deps()
        dest <- check_for_logfile(date)
        cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
        cran %>% filter(package %in% pkgname) %>% 
                group_by(package) %>%
                summarize(n = n())
}    
```
```
num_download(c("filehash", "weathermetrics"))
# A tibble: 2 × 2
  package            n
  <chr>          <int>
1 filehash         179
2 weathermetrics     7
```
# Проверка аргументов
```
num_download <- function(pkgname, date = "2016-07-20") {
        check_pkg_deps()

        ## Check arguments
        if(!is.character(pkgname))
                stop("'pkgname' should be character")
        if(!is.character(date))
                stop("'date' should be character")
        if(length(date) != 1)
                stop("'date' should be length 1")

        dest <- check_for_logfile(date)
        cran <- read_csv(dest, col_types = "ccicccccci", 
                         progress = FALSE)
        cran %>% filter(package %in% pkgname) %>% 
                group_by(package) %>%
                summarize(n = n())
}    
```
Обратите внимание, что здесь мы решили остановить() и выдать ошибку, если аргумент не имеет подходящего типа. Однако альтернативой было бы простое приведение аргумента к символьному типу с помощью функции as.character().
```
num_download("filehash", c("2016-07-20", "2016-0-21"))
```
# Функция возвращает функцию
```
adder_maker <- function(n){
  function(x){
    n + x
  }
}

add2 <- adder_maker(2)
add3 <- adder_maker(3)

add2(5)
[1] 7
add3(5)
[1] 8
```

# map  {purrr}
## map_chr, map_lgl, map_dbl
Семейство функций map применяет функцию к элементам структуры данных, обычно к списку или вектору. Функция вычисляется один раз для каждого элемента вектора с элементом вектора в качестве первого аргумента функции. Возвращаемое значение того же типа, если структура данных (список или вектор), но с заменой каждого элемента результатом вычисляемой функции с соответствующим элементом в качестве аргумента функции. В пакете purrr функция map() возвращает список, а функции map_lgl(), map_chr() и map_dbl() возвращают векторы логических значений, строк или чисел соответственно.
```
library(purrr)

map_chr(c(5, 4, 3, 2, 1), function(x){
  c("one", "two", "three", "four", "five")[x]
})
[1] "five"  "four"  "three" "two"   "one"  

map_lgl(c(1, 2, 3, 4, 5), function(x){
  x > 3
})
[1] FALSE FALSE FALSE  TRUE  TRUE
```

## map_if
Функция map_if() принимает в качестве аргументов список или вектор, содержащий данные, функцию-предикат, а затем функцию, которую необходимо применить. Функция-предикат — это функция, которая возвращает ИСТИНА или ЛОЖЬ для каждого элемента предоставленного списка или вектора. В случае map_if(): если функция предиката оценивается как ИСТИНА, то функция применяется к соответствующему элементу вектора, однако, если функция предиката оценивается как ЛОЖЬ, то функция не применяется. Функция map_if() всегда возвращает список, поэтому я передаю результат map_if() в unlist(), чтобы он выглядел красивее:
```
map_if(1:5, function(x){
              x %% 2 == 0
            },
            function(y){
              y^2
            }) %>% unlist()
```
Возвращается список
```
> map_if(1:5, function(x){
+     x %% 2 == 0
+ },
+ function(y){
+     y^2
+ })
[[1]]
[1] 1

[[2]]
[1] 4

[[3]]
[1] 3

[[4]]
[1] 16

[[5]]
[1] 5

```
## Последовательность
Цифры
```
seq(100, 500, 100)
[1] 100 200 300 400 500
```
Буквы
```
> letters
 [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x"
[25] "y" "z"
```

## map_at() 
```
map_at(seq(100, 500, 100), c(1, 3, 5), function(x){
  x - 10
}) %>% unlist()
[1]  90 200 290 400 490
```

## map2()
### map2_chr
Первые два аргумента должны быть двумя векторами одинаковой длины, за которыми следует функция, которая будет оцениваться с элементом первого вектора в качестве первого аргумента и элементом второго вектора в качестве второго аргумента.
```
map2_chr(letters, 1:26, paste)
 [1] "a 1"  "b 2"  "c 3"  "d 4"  "e 5"  "f 6"  "g 7"  "h 8"  "i 9"  "j 10"
[11] "k 11" "l 12" "m 13" "n 14" "o 15" "p 16" "q 17" "r 18" "s 19" "t 20"
[21] "u 21" "v 22" "w 23" "x 24" "y 25" "z 26"
```
### pmap() 
Семейство функций pmap() похоже на map2(), однако вместо сопоставления двух векторов или списков вы можете отображать любое количество списков. Аргумент списка — это список списков, которые функция будет отображать, за которым следует функция, которая будет применена:
```
pmap_chr(list(
  list(1, 2, 3),
  list("one", "two", "three"),
  list("uno", "dos", "tres")
), paste)
[1] "1 one uno"    "2 two dos"    "3 three tres"
```
# reduce {purrr}
При редукции списка или вектора первый элемент вектора итеративно объединяется со вторым элементом вектора, затем этот комбинированный результат объединяется с третьим элементом вектора и так далее, пока не будет достигнут конец вектора. Применяемая функция должна принимать как минимум два аргумента. Когда отображение возвращает вектор или список, уменьшение должно возвращать одно значение. Ниже приведены некоторые примеры использования функции уменьшения():
```
reduce(c(1, 3, 5, 7), function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  x + y
})
```
Со строками:
```
reduce(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})
x is a
y is b

x is ab
y is c

x is abc
y is d

[1] "abcd"
```
## reduce_right()
```
reduce_right(letters[1:4], function(x, y){
  message("x is ", x)
  message("y is ", y)
  message("")
  paste0(x, y)
})
x is d
y is c

x is dc
y is b

x is dcb
y is a

[1] "dcba"
```
# Search
```
contains(letters, "a")
[1] TRUE
contains(letters, "A")
[1] FALSE
```
# Detect() 
ФункцияDetect() принимает вектор и функцию-предикат в качестве аргументов и возвращает первый элемент вектора, для которого функция-предикат возвращает ИСТИНА:
```
detect(20:40, function(x){
  x > 22 && x %% 2 == 0
})
[1] 24
```
# detect_index()
Функция detect_index() принимает те же аргументы, однако возвращает индекс предоставленного вектора, который содержит первый элемент, удовлетворяющий функции предиката:
```
detect_index(20:40, function(x){
  x > 22 && x %% 2 == 0
})
[1] 5

```
# Фильтр
Группа функций, в которую входят функции keep(), discard(), every() и some(), известна как функция фильтра. 
```
keep(1:20, function(x){
  x %% 2 == 0
})
 [1]  2  4  6  8 10 12 14 16 18 20
```
Функция discard() работает аналогично, она возвращает только те элементы, которые не удовлетворяют функции предиката:
```
discard(1:20, function(x){
  x %% 2 == 0
})
 [1]  1  3  5  7  9 11 13 15 17 19
```
Функция Every() возвращает ИСТИНА только в том случае, если каждый элемент вектора удовлетворяет функции предиката
```
> every(1:20, function(x){
+     x %% 2 == 0
+ })
[1] FALSE
```
 функция some() возвращает ИСТИНА, если хотя бы один элемент вектора удовлетворяет функции предиката
 ```
> some(1:20, function(x){
+     x %% 2 == 0
+ })
[1] TRUE
```

# Compose
функция compose() объединяет любое количество функций в одну:
```
n_unique <- compose(length, unique)
# The composition above is the same as:
# n_unique <- function(x){
#   length(unique(x))
# }

rep(1:5, 1:5)
 [1] 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5

n_unique(rep(1:5, 1:5))
[1] 5
```

# Частичное применение
Частичное применение функций может позволить функциям вести себя как структуры данных. Используя функцию partial() из пакета purrr, вы можете указать некоторые аргументы функции, и тогда partial() вернет функцию, которая принимает только неуказанные аргументы. Давайте рассмотрим простой пример:
```
library(purrr)

mult_three_n <- function(x, y, z){
  x * y * z
}

mult_by_15 <- partial(mult_three_n, x = 3, y = 5)

mult_by_15(z = 4)
[1] 60
```

# Побочные эффекты
Побочные эффекты функций возникают всякий раз, когда функция взаимодействует с «внешним миром» — чтение или запись данных, вывод на консоль и отображение графика — все это побочные эффекты. Побочные эффекты — одна из главных мотиваций для написания кода! Однако с побочными эффектами может быть сложно справиться, поскольку часто имеет значение порядок, в котором выполняются функции с побочными эффектами, и существуют переменные, которые являются внешними по отношению к программе (относительное расположение некоторых данных). Если вы хотите оценить функцию в структуре данных, вы должны использовать функцию walk() из purrr. Вот простой пример:
```
library(purrr)

walk(c("Friends, Romans, countrymen,",
       "lend me your ears;",
       "I come to bury Caesar,", 
       "not to praise him."), message)
Friends, Romans, countrymen,
lend me your ears;
I come to bury Caesar,
not to praise him.
```
# Рекурсия
Представьте, что вы хотите написать функцию, которая складывает вместе все числа в векторе. Конечно, вы можете сделать это с помощью цикла:
```
vector_sum_loop <- function(v){
  result <- 0
  for(i in v){
    result <- result + i
  }
  result
}

vector_sum_loop(c(5, 40, 91))
[1] 136
```
с помощью рекурсии
```
vector_sum_rec <- function(v){
  if(length(v) == 1){
    v
  } else {
    v[1] + vector_sum_rec(v[-1])
  }
}

vector_sum_rec(c(5, 40, 91))
[1] 136
```
Фибоначи
```
fib <- function(n){
  stopifnot(n > 0)
  if(n == 1){
    0
  } else if(n == 2){
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

map_dbl(1:12, fib)
 [1]  0  1  1  2  3  5  8 13 21 34 55 89
```
# Memoization - мемоизация
Ниже приведен пример функции, которая может вычислить первые 25 чисел Фибоначчи. Сначала мы создадим очень простую таблицу, которая представляет собой просто вектор, содержащий 0, 1, а затем 23 NA. Сначала функция fib_mem() проверит, есть ли число в таблице, и если да, то оно будет возвращено. В противном случае число Фибоначчи вычисляется рекурсивно и сохраняется в таблице. Обратите внимание, что мы используем сложный оператор присваивания <<- для изменения таблицы вне области действия функции. Вы узнаете больше о сложном операторе в разделе Выражения и окружения 
```
fib_tbl <- c(0, 1, rep(NA, 23))

fib_mem <- function(n){
  stopifnot(n > 0)
  
  if(!is.na(fib_tbl[n])){
    fib_tbl[n]
  } else {
    fib_tbl[n - 1] <<- fib_mem(n - 1)
    fib_tbl[n - 2] <<- fib_mem(n - 2)
    fib_tbl[n - 1] + fib_tbl[n - 2]
  }
}

map_dbl(1:12, fib_mem)
 [1]  0  1  1  2  3  5  8 13 21 34 55 89
```
# microbenchmark
```
microbenchmark.R
```
# Выражения
Выражения — это инкапсулированные операции, которые может выполнять R. Это может показаться сложным, но использование выражений позволяет вам манипулировать кодом с помощью кода! Вы можете создать выражение, используя функцию thequote(). В качестве аргумента этой функции просто введите то, что вы обычно вводите в консоли R. Например:
```
two_plus_two <- quote(2 + 2)
two_plus_two
2 + 2
eval(two_plus_two)
[1] 4
```
Вы можете столкнуться с кодом R, который хранится в виде строки, которую вы хотите оценить с помощью eval(). Вы можете использовать parse() для преобразования строки в выражение:
```
tpt_string <- "2 + 2"

tpt_expression <- parse(text = tpt_string)

eval(tpt_expression)
[1] 4
```
Вы можете обратить этот процесс и преобразовать выражение в строку, используя функцию deparse():
```
deparse(two_plus_two)
[1] "2 + 2"
```
Одна интересная особенность выражений заключается в том, что вы можете получать доступ к их содержимому и изменять его так же, как и list(). Это означает, что вы можете изменить значения в выражении или даже функцию, выполняемую в выражении, до того, как оно будет вычислено:
```
sum_expr <- quote(sum(1, 5))
eval(sum_expr)
[1] 6
sum_expr[[1]]
sum
sum_expr[[2]]
[1] 1
sum_expr[[3]]
[1] 5
sum_expr[[1]] <- quote(paste0)
sum_expr[[2]] <- quote(4)
sum_expr[[3]] <- quote(6)
eval(sum_expr)
[1] "46"
```
Вы можете составлять выражения, используя функцию call(). Первый аргумент — это строка, содержащая имя функции, за которой следуют аргументы, которые будут переданы этой функции.
```
sum_40_50_expr <- call("sum", 40, 50)
sum_40_50_expr
sum(40, 50)
eval(sum_40_50_expr)
[1] 90
```
Вы можете зафиксировать выражение, которое пользователь R ввел в консоль R при выполнении функции, включив match.call() в функцию, которую выполнил пользователь:
```
return_expression <- function(...){
  match.call()
}

return_expression(2, col = "blue", FALSE)
return_expression(2, col = "blue", FALSE)
```
Конечно, вы можете манипулировать этим выражением внутри функции, которую вы пишете. В приведенном ниже примере сначала используется match.call() для захвата выражения, введенного пользователем. Затем извлекается и оценивается первый аргумент функции. Если первое выражение является числом, то возвращается строка, описывающая первый аргумент, иначе строка «Первый аргумент не числовой». возвращается.
```
first_arg <- function(...){
  expr <- match.call()
  first_arg_expr <- expr[[2]]
  first_arg <- eval(first_arg_expr)
  if(is.numeric(first_arg)){
    paste("The first argument is", first_arg)
  } else {
    "The first argument is not numeric."
  }
}

first_arg(2, 4, "seven", FALSE)
[1] "The first argument is 2"

first_arg("two", 4, "seven", FALSE)
[1] "The first argument is not numeric."
```
# Environments
Окружения — это структуры данных в R, обладающие особыми свойствами в отношении их роли в том, как выполняется код R и как организована память в R. Вы можете этого не осознавать, но вы, вероятно, уже знакомы с одной средой, называемой глобальной средой. Среды формализуют отношения между именами переменных и их значениями. Когда вы вводите x <- 55 в консоль R, вы говорите: присвойте значение 55 переменной с именем x и сохраните это назначение в глобальной среде. Таким образом, глобальная среда — это место, где большинство пользователей R выполняют большую часть своего программирования и анализа.

Вы можете создать новую среду, используя new.env(). Вы можете назначать переменные в этой среде аналогично назначению именованного элемента списка, или вы можете использовать assign(). Вы можете получить значение переменной точно так же, как вы извлекаете именованный элемент списка, или вы можете использовать get(). Обратите внимание, что assign() и get() противоположны:
```
my_new_env <- new.env()
my_new_env$x <- 4
my_new_env$x
[1] 4

assign("y", 9, envir = my_new_env)
get("y", envir = my_new_env)
[1] 9
my_new_env$y
[1] 9
```
Вы можете получить все имена переменных, которые были назначены в среде, используя ls(), вы можете удалить связь между именем переменной и значением, используя rm(), и вы можете проверить, было ли присвоено имя переменной в среда с использованием exists():
```
ls(my_new_env)
[1] "x" "y"
rm(y, envir = my_new_env)
exists("y", envir = my_new_env)
[1] TRUE
exists("x", envir = my_new_env)
[1] TRUE
my_new_env$x
[1] 4
my_new_env$y
NULL
```
Среды организованы в отношениях родитель/потомок, так что каждая среда отслеживает свою родительскую среду, но родители не знают, какие среды являются их дочерними. Обычно отношения между средами не являются чем-то, что вы должны пытаться контролировать напрямую. Вы можете увидеть родителей глобальной среды, используя функциюsearch():
```
search()
 [1] ".GlobalEnv"             "package:magrittr"      
 [3] "package:tidyr"          "package:microbenchmark"
 [5] "package:purrr"          "package:dplyr"         
 [7] "package:readr"          "package:stats"         
 [9] "package:graphics"       "package:grDevices"     
[11] "package:utils"          "package:datasets"      
[13] "Autoloads"              "package:base"          
```
Как видите, package:magrittr является родителем .GlobalEnv, а package:tidyr является родителем package:magrittr и так далее. Как правило, родителем .GlobalEnv всегда является последний пакет, загруженный с помощью библиотеки(). Обратите внимание, что после загрузки пакета ggplot2 этот пакет становится родителем .GlobalEnv:
```
library(ggplot2)
search()
```

# Среды выполнения
Хотя может быть несколько случаев, когда вам нужно создать новую среду с помощью new.env(), вы чаще будете создавать новые среды всякий раз, когда выполняете функции. Среда выполнения — это среда, временно существующая в рамках выполняемой функции. Например, если у нас есть следующий код:
```
x <- 10

my_func <- function(){
  x <- 5
  return(x)
}

my_func()
```
Как вы думаете, что будет результатом my_func()? Сделайте свое предположение, а затем взгляните на исполняемый код ниже:
```
x <- 10

my_func <- function(){
  x <- 5
  return(x)
}

my_func()
[1] 5
```
Так что же именно происходит выше? Сначала имени x присваивается значение 10 в глобальной среде. Затем namemy_func присваивается значение функции function(){x <- 5};return(x)} в глобальной среде. Когда my_func() выполняется, создается новая среда, называемая средой выполнения, которая существует только во время работы my_func(). Внутри среды выполнения имени x присваивается значение 5. При выполнении return() сначала ищет в среде выполнения значение, присвоенное x. Затем возвращается значение 5. В отличие от ситуации выше, взгляните на этот вариант:
```
x <- 10

another_func <- function(){
  return(x)
}

another_func()
[1] 10
```
В этой ситуации среда выполнения внутри other_func() не содержит присвоения имени x, поэтому R ищет присвоение в родительской среде среды выполнения, которая является глобальной средой. Поскольку x присваивается значение 10, в глобальной среде возвращается 10.

После просмотра приведенных выше случаев вам может быть любопытно, возможно ли для среды выполнения манипулировать глобальной средой. Вы уже знакомы с оператором присваивания <-, однако вы также должны знать, что существует еще один оператор присваивания, называемый сложным оператором присваивания, который выглядит как <<-. Вы можете использовать сложный оператор присваивания, чтобы переназначить или даже создать привязки "имя-значение" в глобальной среде из среды выполнения. В этом первом примере функция assign1() изменит значение, связанное с именем x:
```
x <- 10
x
[1] 10

assign1 <- function(){
  x <<- "Wow!"
}

assign1()
x
[1] "Wow!"
```
Вы можете видеть, что значение, связанное с x, изменилось с 10 на «Вау!» в глобальной среде. Вы также можете использовать <<- для присвоения имен значениям, которые еще не были определены в глобальной среде внутри функции :
```
a_variable_name
Error in eval(expr, envir, enclos): object 'a_variable_name' not found
exists("a_variable_name")
[1] FALSE

assign2 <- function(){
  a_variable_name <<- "Magic!"
}

assign2()
exists("a_variable_name")
[1] TRUE
a_variable_name
[1] "Magic!"
```
# Что такое ошибка?
Ошибки чаще всего возникают, когда код используется не по назначению. Например, сложение двух строк вместе приводит к следующей ошибке:
```
"hello" + "world"
Error in "hello" + "world": non-numeric argument to binary operator
```
Оператор + — это, по сути, функция, которая принимает два числа в качестве аргументов и находит их сумму. Поскольку ни «привет», ни «мир» не являются числами, интерпретатор R выдает ошибку. Ошибки остановят выполнение вашей программы и (надеюсь) выведут сообщение об ошибке в консоль R.

В R есть две другие конструкции, обе связанные с ошибками: предупреждения и сообщения. Предупреждения предназначены для указания на то, что что-то пошло не так в вашей программе, которую следует проверить. Вот простой пример генерируемого предупреждения:
```
as.numeric(c("5", "6", "seven"))
Warning: NAs introduced by coercion
[1]  5  6 NA
```
Функция as.numeric() пытается преобразовать каждую строку в c("5", "6", "семь") в число, однако преобразовать "семь" невозможно, поэтому генерируется предупреждение. Выполнение кода не останавливается, и вместо числа вместо «семерки» выдается NA.

Сообщения просто выводят тест на консоль R, хотя они генерируются базовым механизмом, аналогичным тому, как генерируются ошибки и предупреждения. Вот небольшая функция, которая будет генерировать сообщение:
```
f <- function(){
  message("This is a message.")
}

f()
This is a message.

```
# Генерация ошибок
В R есть несколько важных функций для генерации ошибок, предупреждений и сообщений. Функция stop() генерирует ошибку. Сгенерируем ошибку:
```
stop("Something erroneous has occured!")
```
результат:
```
Error: Something erroneous has occured!
```
Если внутри функции возникает ошибка, имя этой функции появится в сообщении об ошибке:
```
name_of_function <- function(){
  stop("Something bad happened.")
}

name_of_function()
Error in name_of_function(): Something bad happened.
```
Функция stopifnot() принимает ряд логических выражений в качестве аргументов, и если какое-либо из них ложно, генерируется ошибка, указывающая, какое выражение ложно. Давайте рассмотрим пример:
```
error_if_n_is_greater_than_zero <- function(n){
  stopifnot(n <= 0)
  n
}

error_if_n_is_greater_than_zero(5)
Error: n <= 0 is not TRUE
```
Функция warning() создает предупреждение, а сама функция очень похожа на функцию stop(). Помните, что предупреждение не останавливает выполнение программы (в отличие от ошибки).
```
warning("Consider yourself warned!")
Warning: Consider yourself warned!
```
Как и ошибки, предупреждение, сгенерированное внутри функции, будет включать имя функции, в которой оно было сгенерировано:

Сообщения проще, чем ошибки или предупреждения, они просто выводят строки в консоль R. Вы можете отправить сообщение с помощью функции themessage():
```
make_NA <- function(x){
  warning("Generating an NA.")
  NA
}

make_NA("Sodium")
Warning in make_NA("Sodium"): Generating an NA.
[1] NA
```
# Как следует обрабатывать ошибки?
Функция tryCatch() — это рабочая лошадка для обработки ошибок и предупреждений в R. Первым аргументом этой функции является любое выражение R, за которым следуют условия, определяющие, как обрабатывать ошибку или предупреждение. Последний аргумент, наконец, определяет функцию или выражение, которое будет выполняться после выражения, несмотря ни на что, даже в случае ошибки или предупреждения.
```
beera <- function(expr){
  tryCatch(expr,
         error = function(e){
           message("An error occurred:\n", e)
         },
         warning = function(w){
           message("A warning occured:\n", w)
         },
         finally = {
           message("Finally done!")
         })
}
```
```
beera({
  2 + 2
})
Finally done!
[1] 4

beera({
  "two" + 2
})
An error occurred:
Error in "two" + 2: non-numeric argument to binary operator

Finally done!

beera({
  as.numeric(c(1, "two", 3))
})
A warning occured:
simpleWarning in doTryCatch(return(expr), name, parentenv, handler): NAs introduced by coercion

Finally done!
```
Обратите внимание, что мы эффективно преобразовали ошибки и предупреждения в сообщения.

Теперь, когда вы знаете основы генерации и перехвата ошибок, вам нужно решить, когда ваша программа должна генерировать ошибку. Мой вам совет — максимально ограничить количество ошибок, которые генерирует ваша программа. Даже если вы разрабатываете свою программу так, чтобы она могла перехватывать и обрабатывать ошибки, процесс обработки ошибок замедляет вашу программу на порядки. Представьте, что вы хотите написать простую функцию, которая проверяет, является ли аргумент четным числом. Вы можете написать следующее:
```
is_even <- function(n){
  n %% 2 == 0
}

is_even(768)
[1] TRUE

is_even("two")
Error in n%%2: non-numeric argument to binary operator
```
Вы можете видеть, что предоставление строки приводит к тому, что эта функция вызывает ошибку. Однако вы можете представить, что хотите использовать эту функцию для списка различных типов данных, и вам нужно знать только, какие элементы этого списка являются четными числами. Вы можете подумать написать следующее:
```
is_even_error <- function(n){
  tryCatch(n %% 2 == 0,
           error = function(e){
             FALSE
           })
}

is_even_error(714)
[1] TRUE

is_even_error("eight")
[1] FALSE
```
Похоже, это работает так, как вы предполагали, однако при применении к большему количеству данных эта функция будет очень медленной по сравнению с альтернативами. Например, я мог бы проверить, является ли n числом, прежде чем обрабатывать n как число:
```
is_even_check <- function(n){
  is.numeric(n) && n %% 2 == 0
}

is_even_check(1876)
[1] TRUE

is_even_check("twelve")
[1] FALSE
```
Обратите внимание, что при использовании `is.numeric()` перед оператором "И" (`&&`) выражение `n %% 2 == 0` никогда не вычисляется. Это особенность дизайна языка программирования, называемая «короткое замыкание». Выражение никогда не может быть оценено как «ИСТИНА», если левая часть «&&» оценивается как «ЛОЖЬ», поэтому правая часть игнорируется.

Чтобы продемонстрировать разницу в скорости кода, мы воспользуемся пакетом microbenchmark, чтобы измерить, сколько времени требуется для применения каждой функции к одним и тем же данным.
```
library(microbenchmark)
microbenchmark(sapply(letters, is_even_check))
microbenchmark(sapply(letters, is_even_error))
```
# Обзор отладки
browser(): интерактивная среда отладки, которая позволяет выполнять код по одному выражению за раз.

debug() / debugonce(): функция, которая запускает браузер внутри функции.

trace(): эта функция позволяет вам временно вставлять фрагменты кода в другие функции, чтобы изменить их поведение.

recovery(): функция для навигации по стеку вызовов функций после того, как функция выдала ошибку.

traceback(): распечатывает стек вызовов функций после возникновения ошибки; ничего не делает, если нет ошибки

## traceback()
При возникновении ошибки проще всего немедленно вызвать функцию traceback(). Эта функция возвращает стек вызовов функций непосредственно перед тем, как произошла ошибка, чтобы вы могли видеть, на каком уровне вызовов функций произошла ошибка. Если у вас есть много функций, которые успешно вызывают друг друга, вывод traceback() может быть полезен для определения того, где копать в первую очередь.
```
check_n_value <- function(n) {
        if(n > 0) {
                stop("n should be <= 0")
        }
}
error_if_n_is_greater_than_zero <- function(n){
        check_n_value(n)
        n
}
error_if_n_is_greater_than_zero(5)
Error in check_n_value(n): n should be <= 0
```
Запуск функции traceback() сразу после получения этой ошибки даст нам
```
traceback()
3: stop("n should be <= 0") at #2
2: check_n_value(n) at #2
1: error_if_n_is_greater_than_zero(5)
```
## Browsing a Function Environment 
Из вывода трассировки часто можно определить, в какой функции и в какой строке кода возникает ошибка. Если вы являетесь автором кода, о котором идет речь, один простой способ — вставить вызов функции browser() рядом с ошибкой (в идеале, до того, как ошибка возникнет). Функция browser() теперь принимает аргументы и просто размещается в любом месте функции. Как только он будет вызван, вы окажетесь в среде браузера, которая очень похожа на обычную рабочую среду R, за исключением того, что вы находитесь внутри функции.
```
check_n_value <- function(n) {
        if(n > 0) {
                browser()  ## Error occurs around here
                stop("n should be <= 0")
        }
}
```
Теперь, когда мы вызовем error_if_n_is_greater_than_zero(5), мы увидим следующее.
```
error_if_n_is_greater_than_zero(5)
Called from: check_n_value(n)
Browse[1]> 
```
## Tracing Functions
Если у вас есть легкий доступ к исходному коду функции (и вы можете изменить код), то обычно проще всего вставлять вызовы browser() непосредственно в код, когда вы отслеживаете различные ошибки. Однако, если у вас нет простого доступа к коду функции или, возможно, функция находится внутри пакета, который требует пересборки после каждого редактирования, иногда проще использовать функцию trace() для временных изменений кода.

Самое простое использование trace() — просто вызвать функцию trace() без каких-либо других аргументов.
```
trace("check_n_value")
```
Теперь всякий раз, когда функция check_n_value() вызывается какой-либо другой функцией, вы увидите сообщение, напечатанное на консоли, указывающее, что функция была вызвана.
```
error_if_n_is_greater_than_zero(5)
trace: check_n_value(n)
Error in check_n_value(n): n should be <= 0
```
Здесь мы видим, что check_n_value() была вызвана один раз перед тем, как произошла ошибка. Но мы можем сделать больше с помощью trace(), например, вставить вызов browser() в определенное место, например, прямо перед вызовом tostop().

Мы можем получить номера выражений каждой части функции, вызвав as.list() в теле() функции.
```
as.list(body(check_n_value))
[[1]]
`{`

[[2]]
if (n > 0) {
    stop("n should be <= 0")
}
```
Здесь оператор if — это второе выражение в функции (первое «выражение» — это самое начало функции). Далее мы можем разбить второе выражение следующим образом.

```
as.list(body(check_n_value)[[2]])
[[1]]
`if`

[[2]]
n > 0

[[3]]
{
    stop("n should be <= 0")
}
```
Теперь мы видим, что вызов stop() является третьим подвыражением во втором выражении общей функции. Мы можем указать это для trace(), передав целочисленный вектор, заключенный в список, в аргумент at.
```
trace("check_n_value", browser, at = list(c(2, 3)))
[1] "check_n_value"
```
Функция trace() имеет побочный эффект изменения функции и преобразования в новый объект класса «functionWithTrace».
```
check_n_value
Object with tracing code, class "functionWithTrace"
Original definition: 
function(n) {
        if(n > 0) {
                stop("n should be <= 0")
        }
}

## (to see the tracing code, look at body(object))
```
Здесь мы видим, что код был изменен, чтобы добавить вызов browser() непосредственно перед вызовом tostop().

Мы можем добавить в функцию более сложные выражения, обернув их вызовом quote() внутри функции trace(). Например, мы можем захотеть вызвать только определенное поведение в зависимости от локальных условий функции.
```
trace("check_n_value", quote({
        if(n == 5) {
                message("invoking the browser")
                browser()
        }
}), at = 2)
[1] "check_n_value"
```

Отладка функций внутри пакета — еще один ключевой вариант использования trace(). Например, если мы хотим вставить код трассировки в функцию glm() в пакете stats, единственное дополнение к вызову trace(), которое нам потребуется, — предоставить информацию о пространстве имен через аргумент where.
```
trace("glm", browser, at = 4, where = asNamespace("stats"))
Tracing function "glm" in package "namespace:stats"
[1] "glm"
```
## debug() и debugonce()
Функции debug() и debugonce() можно вызывать для других функций, чтобы включить «состояние отладки» функции. Вызов debug() для функции делает так, что при вызове этой функции вы немедленно входите в браузер и можете выполнять код по одному выражению за раз.
```
## Turn on debugging state for 'lm' function
debug(lm)
``` 
Вызов debug(f), где f — функция, в основном эквивалентен вызову trace(f, browser), который вызовет функцию browser() при входе в функцию.

Состояние отладки является постоянным, поэтому, если функция помечена для отладки, она останется помеченной. Поскольку легко забыть о состоянии отладки функции, функция debugonce() включает состояние отладки при следующем вызове функции, но затем выключает его после выхода из браузера.

## recover()
Функция recovery() используется нечасто, но может быть важным инструментом при отладке сложного кода. Как правило, вы не вызываете recover() напрямую, а устанавливаете ее как функцию, которая будет вызываться всякий раз, когда в коде возникает ошибка. Это можно сделать с помощью функции options().
```
options(error = recover)
```
Обычно, когда в коде возникает ошибка, выполнение кода прекращается, и вы возвращаетесь к обычному приглашению консоли R. Однако, когда используется функция восстановления() и возникает ошибка, вам предоставляется стек вызовов функций и меню.