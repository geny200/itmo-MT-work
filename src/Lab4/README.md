## [Содержание](./../../README.md)

+ [2 lab - Лабораторная работа по ручному построению нисходящих парсеров](./../Lab2/README.md)
+ [3 lab - Лабораторная работа по автоматическим генераторам парсеров](./../Lab3/README.md)
    + [Автоматически сгенерированный код](./../Lab3/README.md)
    + [Грамматика для Python To C](./../../resources/PythonToC/README.md)
+ **4 lab - Лабораторная работа по написанию генераторов парсеров**

***

## <a name="lab4"></a>    4 лабораторная работа

Необходимо написать некоторый упрощенный аналог генератора трансляторов. Рекомендуется брать за основу синтаксис *ANTLR*
или *Bison*. Рекомендуется для чтения входного файла с грамматикой сгенерировать разборщик с помощью *ANTLR* или *Bison*
.
<br/><br/>Необходимо набрать в сумме хотя бы **35 баллов**.
<br/><br/>**Обязательное требование:** должен быть лексический анализатор, не должно быть ограничения, что токен это
один символ.
<br/><br/>Необходимо из каждого пункта выполнить **хотя бы 1 вариант**.

### Выбор класса грамматик:

1. (10 баллов) LL(1)-грамматики, нисходящий разбор
2. (15 баллов) SLR-грамматики, восходящий разбор
3. (20 баллов) LALR-грамматики, восходящий разбор

### Работа с атрибутами

1. (10 баллов) поддержка синтезируемых атрибутов
2. (10 баллов) поддержка наследуемых атрибутов

### Тестирование получившегося генератора

1. (обязательно) сгенерировать с помощью вашего генератора калькулятор
2. (5 баллов) выполнить с помощью вашего генератора ваше задание второй лабораторной
3. (10 баллов) выполнить с помощью вашего генератора ваше задание третьей лабораторной

### Выбранный вариант (40 баллов)

Для выполнения 4 работы были выбраны пункты:

* Выбор класса грамматик
    * LL(1)-грамматики, нисходящий разбор
* Работа с атрибутами
    * поддержка синтезируемых атрибутов
    * поддержка наследуемых атрибутов
* Тестирование получившегося генератора
    * сгенерировать с помощью вашего генератора калькулятор
    * выполнить с помощью вашего генератора ваше задание третьей лабораторной

***

## Комментарии по реализации

В качестве основного языка написания лабораторной работы был выбран *Haskell*. Лабораторная писалась во время изучения
курса функционального программирования (*Haskell*), поэтому применялся разный набор инструментов для реализации (для
освоения на практике пройденного материала).

**Синтаксис** генератора и лексера был взят с небольшими изменениями по образу и подобию генератора
парсеров **[Happy](https://www.haskell.org/happy/)** и лексера **[Alex](https://www.haskell.org/alex/)**
(Аналоги для *ANTLR* или *Bison* и *ANTLR lexer* или *Flex* соответственно, на *Haskell*)

### Лексический анализатор
Расположен в пакете [Lexer](Lexer). Экспортирует функцию для генерации лексера по заданной грамматике, 
который можно использовать в Sheol генераторе

### Генератор парсеров (Sheol генератор)
Расположен в пакете [Sheol](Sheol). Данный генератор был назван *Sheol генератор* (в соответствии с традицией 
именования автоматических генераторов парсеров).

Экспортирует функцию для генерации парсера по данной грамматике.

### Результаты
**Грамматики** для выполнения 3 пункта данной работы располагаются в **[ресурсах проекта](./../../resources)**:
+ Грамматика калькулятора - [Calculator](./../../resources/Calculator)
+ Грамматика Python to C (3 лабораторная) - [Python to C](./../../resources/PythonToC)

**Автоматически сгенерированный код** для заданных грамматик находится в пакете [**Lab3**](./../Lab3):
+ Автоматически сгенерированный код для калькулятора - [Calculator](./../Lab3/Calculator)
+ Автоматически сгенерированный код для Python to C - [Python to C](./../Lab3/PythonToC)