# [Содержание](./../../README.md)

+ [2 lab - Лабораторная работа по ручному построению нисходящих парсеров](./../Lab2/README.md)
+ **3 lab - Лабораторная работа по автоматическим генераторам парсеров**
    + **Автоматически сгенерированный код**
    + [Грамматика для Python To C](./../../resources/PythonToC/README.md)
+ [4 lab - Лабораторная работа по написанию генераторов парсеров](./../Lab4/README.md)

***

# 3 лабораторная работа

## Введение

Цель данной лабораторной работы — научиться пользоваться автоматическими генераторами анализаторов *Bison* и *ANTLR*.
<br/><br/>**Форма отчетности:** программа и текстовый отчет. Средство автоматической генерации вы можете выбрать
самостоятельно.
<br/><br/>**Рекомендуемые источники:**

+ http://www.gnu.org/software/bison — Bison
+ http://www.antlr.org — ANTLR

## Вариант 3 - Перевод с Python на Си

Выберите подмножество языка *Python* и напишите транслятор, который переводит программы на заданном подмножестве на
язык *Си*.
<br/><br/>Вы можете выбрать небольшое подмножество языка, но на входе и на выходе вашего транслятора должны быть
компилирующиеся программы.
<br/><br/>**Пример:**

```
a = int(input())
b = int(input())
print(a + b)
```

**Вывод:**

```
int a, b;
int main() {
    scanf("%d", &a);
    scanf("%d", &b);
    printf("%d\n", a + b);
    return 0;
}
```

***

## Комментарии по реализации

В качестве основного языка написания лабораторной работы был выбран *Haskell*. Лабораторная писалась во время изучения
курса функционального программирования (*Haskell*), поэтому применялся разный набор инструментов для реализации (для
освоения на практике пройденного материала).

**Важное замечание:** В данном пакете лежит уже автоматически **сгенерированный код** по грамматике с помощью генератора
из [лабораторной работы 4 (**Sheol generator**)](./../Lab4/README.md), грамматика местами наследует синтаксис *Haskell*.

#### Грамматика расположена в **[ресурсах проекта](./../../resources/PythonToC/README.md)**:

+ [Грамматика: Python to C - lexer](./../../resources/PythonToC/PyToC.lex)
+ [Грамматика: Python to C - gramma](./../../resources/PythonToC/PyToC.ly)

#### Автоматически сгенерированный код (Python to C):

+ [Lexer - Python to C](PythonToC/Lexer.hs) - экспортирует лексер для разбора строки в список токенов
+ [Parser - Python to C](PythonToC/Parser.hs) - экспортирует синтаксический анализатор для разбора и преобразования кода
  на `Python` в код на `C`.

**Замечание:** В данном пакете так же находится автоматически сгенерированный код для **калькулятора**
(одно из заданий [4-ой лабораторной работы](./../Lab4/README.md))

  