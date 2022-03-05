# Семинар 03.03.2022

На семинаре мы реализовали простое графическое приложение с использованием библиотеки
[`gloss`](https://hackage.haskell.org/package/gloss)

Основные функции:
[`play`](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-Pure-Game.html#v:play)
из модуля
[Graphics.Gloss.Interface.Pure.Game](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-Pure-Game.html) и
[`playIO`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#v:playIO) из
[Graphics.Gloss.Interface.IO.Game](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html).

У них одинаковые параметры:

- режим окна (тип [`Display`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Display.html#t:Display))
- цвет фона (тип [`Color`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Color.html#t:Color))
- количество шагов симуляции в секунду
- начальное состояние игрового мира (имеет полиморфный тип, который задаётся программистом)
- функция отрисовки игрового мира (переводит мир в картинку типа [`Picture`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html#t:Picture))
- функция обработки событий (преобразует игровой мир в зависимости от произошедшего внешнего события)
  типа [`Event`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:Event))
- функция обновления игрового мира за промежуток времени (прошедшее время в секундах передаётся
  как первый параметр)

Основная часть реализации находится в функциях отрисовки, обновления и обработки события.

В `play` эти три функции чистые. В них не может быть
побочных эффектов и общения с внешним миром. По возможности используйте этот вариант, совершив
все предварительные действия (такие как загрузка конфигурационного файла или изображений)
до старта игры.

В `playIO` в этих функциях допустимы побочные эффекты. Без них не обойтись, если, например,
по нажатию клавиши игра должна сохраняться в файл.

Цвета в библиотеке представляются в модели RGBA, основные цвета уже определены в
библиотеке в виде констант с говорящими именами (`red`, `green`, `yellow` и т.д.).
Функции для работы с цветом перечислены в модуле [Graphics.Gloss.Data.Color](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Color.html).

Внешнее событие описывается типом [`Event`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:Event).
Это может быть либо нажатие клавиши, либо изменение положения курсора, либо изменение размера окна.
Чаще всего для обработки нажатия клавиши достаточно определить саму клавишу
(значение типа [`Key`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#t:Key))
и какое было состояние клавиши (её можно нажать или отпустить -- это два разных события!).
Для этого удобно использовать сопоставление с образцом в функции-обработчике.

Для рендеринга изображений используйте конструкторы значений для типа [`Picture`](http://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html). Часть из них описывает форму изображения (линия, прямоугольник, круг, текст и т.д.), часть
конструкторов рекурсивна и задаёт преобразование изображений (изменение цвета, расположения или масштаба) и
объединение изображений в одно (конструктор `Pictures`). Для использования произвольных изображений
используйте конструктор значений `Bitmap`, в который нужно передать внутренне представление
изображения типа `BitmapData`. Для того чтобы загрузить изображение из файла и получить такое
внутреннее представление, можно использовать библиотеку [gloss-juicy](https://hackage.haskell.org/package/gloss-juicy).
