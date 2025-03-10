open System
open System.IO

// Функция для проверки ввода целого числа
let rec checkInt (prompt: string) =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Int64.TryParse(input) with
    | true, number ->
        if number <= int64 Int32.MaxValue && number >= 1L then
            Some (int number)
        else
            printfn "Ошибка: введите натуральное число (целое число больше 0 и не превышающее 2147483647)."
            checkInt prompt
    | false, _ ->
        printfn "Введите целое число."
        checkInt prompt

// Добавление символа к каждой строке
let addCharToSeq (charToAdd: char) (strings: string list) =
    strings |> List.map (fun str -> str + string charToAdd)

// Генерация случайной строки
let generateRandomString (length: int) =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+:'№,.;<>{}|/[]?`~"
    String(Array.init length (fun _ -> chars.[random.Next(chars.Length)]))

// Генерация списка случайных строк
let generateRandomStrings (count: int) (length: int) =
    Seq.init count (fun _ -> generateRandomString length) |> List.ofSeq

// Ввод строк вручную
let inputStringsManually (count: int) =
    Seq.init count (fun i ->
        printfn "Введите строку #%d:" (i + 1)
        Console.ReadLine()) |> List.ofSeq

// случайный символ
let generateRandomChar () =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+:'№,.;<>{}|/[]?`~"
    chars.[random.Next(chars.Length)]

// Запрос способа ввода строк
let requestInputMethod () =
    let rec loop () =
        printfn "Как ввести строки? 1 - вручную, 2 - случайно:"
        let input = Console.ReadLine()
        match input with
        | "1" -> input
        | "2" -> input
        | _ ->
            printfn "Неверный ввод. Пожалуйста, введите '1' или '2'."
            loop ()
    loop ()

// Запрос способа ввода символа
let requestCharInputMethod () =
    let rec loop () =
        printfn "Как ввести символ? 1 - вручную, 2 - случайно:"
        let input = Console.ReadLine()
        match input with
        | "1" -> input
        | "2" -> input
        | _ ->
            printfn "Неверный ввод. Пожалуйста, введите '1' или '2'."
            loop ()
    loop ()

// Задание 1
let z1 () =
    let inputMethod = requestInputMethod ()

    let strings =
        match inputMethod with
        | "1" ->
            // Ввод вручную
            let count = checkInt "Введите количество строк: " |> Option.get
            inputStringsManually count
        | "2" ->
            // Случайным образом
            let count = checkInt "Введите количество строк: " |> Option.get
            let length = checkInt "Введите длину каждой строки: " |> Option.get
            generateRandomStrings count length
        | _ -> failwith "Не должно достигаться (проверка уже выполнена)"

    // Вывод строк
    printfn "Введенные строки:"
    strings |> List.iter (printfn "%s")

    // Ввод символа
    let charInputMethod = requestCharInputMethod ()

    let charToAdd =
        match charInputMethod with
        | "1" ->
            // Ввод символа вручную
            let rec getValidChar () =
                printfn "Введите символ для добавления:"
                let input = Console.ReadLine()
                if input.Length = 1 then input.[0]
                else
                    printfn "Ошибка: необходимо ввести ровно один символ."
                    getValidChar ()
            getValidChar ()
        | "2" ->
            // Генерация случайного символа
            generateRandomChar ()
        | _ -> failwith "Не должно достигаться (проверка уже выполнена)"

    // Добавление символа к строкам
    let resultStrings = addCharToSeq charToAdd strings

    // Вывод результата
    printfn "Строки после добавления символа:"
    resultStrings |> List.iter (printfn "%s")
    ()

// Задание 2
let z2 () =
    let count = checkInt "Введите количество строк: " |> Option.get
    let strings = inputStringsManually count

    printfn "Введенные строки:"
    strings |> List.iter (printfn "%s")

    // Проверка, если все строки пустые
    if strings |> List.forall (fun str -> str = "") then
        printfn "Все строки пустые."
    else
        // Поиск самой короткой строки
        let shortestString =
            strings
            |> List.fold (fun shortest str ->
                if String.length str < String.length shortest then str else shortest) strings.[0]

        // Вывод результата
        if shortestString = "" then
            printfn "Самая короткая строка: (пустая строка)"
        else
            printfn "Самая короткая строка: %s" shortestString
    ()

// Задание 3
let z3 () =
    printfn "Введите путь к каталогу:"
    let path = Console.ReadLine()

    if not (Directory.Exists path) then
        printfn "Ошибка: Каталог '%s' не существует." path
    else
        try
            let files = Directory.GetFiles(path)
            let sortedFiles = 
                files 
                |> Array.map Path.GetFileName
                |> Array.sortBy (fun name -> name.ToLower()) 
            
            match sortedFiles with
            | [||] -> printfn "В каталоге нет файлов."
            | arr -> 
                let lastFile = arr.[arr.Length - 1]
                printfn "Последний файл по алфавиту: %s" lastFile
        with
        | ex ->
            printfn "Произошла ошибка: %s" ex.Message

// Основная функция
let main () =
    let rec loop () =
        printfn "Введите номер программы для проверки: "
        printfn "1 - Seq.map"
        printfn "2 - Seq.fold"
        printfn "3 - Последний по алфавиту файл в каталоге"
        printfn "0 - Выход из программы"
        printf "Введите номер задачи: "
        let input = Console.ReadLine()
        match input with
        | "1" -> z1 ()
        | "2" -> z2 ()
        | "3" -> z3 ()
        | "0" -> printfn "Выход из программы..."
        | _ -> printfn "Неверный ввод! Пожалуйста, выберите число от 0 до 3."
        if input <> "0" then loop ()
    loop ()

// Запуск программы
main ()