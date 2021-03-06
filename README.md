# Лабораторные по Методам Трансляции

## Содержание

+ [2 lab - Лабораторная работа по ручному построению нисходящих парсеров](src/Lab2/README.md)
+ [3 lab - Лабораторная работа по автоматическим генераторам парсеров](src/Lab3/README.md)
    + [Автоматически сгенерированный код](src/Lab3/README.md)
    + [Грамматика для Python To C](resources/PythonToC/README.md)
+ [4 lab - Лабораторная работа по написанию генераторов парсеров](src/Lab4/README.md)

***

## <a name="lab2"></a>    [2 лабораторная работа ](src/Lab2/README.md)

### Вариант 3 - Логические формулы в стиле Python

Используются операции `and`, `or`, `xor`, `not`. Приоритет операций стандартный. Скобки могут использоваться для
изменения приоритета.
<br/><br/>В качестве операндов выступают переменные с именем из одной буквы. Используйте один терминал для всех
переменных. Для каждой логической операции должен быть заведен один терминал (не три `‘a’`,`‘n’`,`‘d’` для `and`).
<br/><br/>**Пример:** `(a and b) or not (c xor (a or not b))`
***

## <a name="lab3"></a>    [3 лабораторная работа ](src/Lab3/README.md)

### Вариант 3 - Перевод с Python на Си

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

## <a name="lab4"></a>    [4 лабораторная работа ](src/Lab4/README.md)

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
