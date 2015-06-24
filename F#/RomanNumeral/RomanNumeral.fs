namespace RomanNumeral

module RomanNumeral =

    let ExtractDigit power number = number / power % 10 
    
    type Mapping = {One:string; Five:string; Ten:string; Power:int} with
        member this.PrintOne = this.One
        member this.PrintFour = this.One + this.Five
        member this.PrintFive = this.Five
        member this.PrintNine = this.One + this.Ten
        member this.PrintSmallDigit d = String.replicate d this.One
        member this.PrintBigDigit d = this.Five + this.PrintSmallDigit (d-5)
        member this.PrintDigit number =
            let digit = ExtractDigit this.Power number 
            match digit with
            | d when d <= 3 -> this.PrintSmallDigit d
            | 4 -> this.PrintFour
            | d when d >= 5 && d <= 8 -> this.PrintBigDigit d
            | 9 -> this.PrintNine

    let thousandPattern = {One = "M"; Five = "?"; Ten = "?"; Power=1000 }
    let hundredPattern = {One = "C"; Five = "D"; Ten = thousandPattern.One; Power=100 }
    let tenPattern = {One = "X"; Five = "L"; Ten = hundredPattern.One; Power=10 }
    let unitPattern = {One = "I"; Five = "V"; Ten = tenPattern.One; Power=1 }

    let ToRoman number =
        thousandPattern.PrintDigit number +
        hundredPattern.PrintDigit number +
        tenPattern.PrintDigit number +
        unitPattern.PrintDigit number

//    let PrintDigit2 number currentText (mapping:Mapping)= 
//        currentText + mapping.PrintDigit number 
//
//    let patterns = [
//        thousandPattern;
//        hundredPattern;
//        tenPattern;
//        unitPattern;
//        ]
//
//    let ToRomanBis number = 
//        let PrintPattern = PrintDigit2 number
//        List.fold PrintPattern "" patterns
//    let ToRoman = ToRomanBis

    let (|Prefix|_|) (prefix:string) (text:string) = 
        if text.StartsWith(prefix) then
            Some(text.Substring(prefix.Length))
        else 
            None

    type ArabicBuilder = {UnparsedText:string; Number:int}

    let rec ReadDigit (mapping:Mapping) builder =
         let again = ReadDigit mapping
         match builder.UnparsedText with
         | Prefix mapping.PrintNine remainingText -> {UnparsedText = remainingText; Number = builder.Number + (9 * mapping.Power)} |> again
         | Prefix mapping.PrintFive remainingText -> {UnparsedText = remainingText; Number = builder.Number + (5 * mapping.Power)} |> again
         | Prefix mapping.PrintFour remainingText -> {UnparsedText = remainingText; Number = builder.Number + (4 * mapping.Power)} |> again
         | Prefix mapping.PrintOne remainingText -> {UnparsedText = remainingText; Number = builder.Number + (1 * mapping.Power)} |> again
         | _ -> builder

    let ReadUnit = ReadDigit unitPattern
    let ReadTenth = ReadDigit tenPattern
    let ReadHundreds = ReadDigit hundredPattern
    let ReadThousands = ReadDigit thousandPattern

    let ToArabic text =
        let builtNumber =
            {UnparsedText = text; Number = 0}
            |> ReadThousands
            |> ReadHundreds
            |> ReadTenth
            |> ReadUnit
        builtNumber.Number

namespace Tests

open NUnit.Framework
open RomanNumeral.RomanNumeral

[<TestFixture>]
type RomanNumeralToRoman() = 
    [<TestCase(0, Result="")>]
    [<TestCase(1, Result="I")>]
    [<TestCase(5, Result="V")>]
    [<TestCase(10, Result="X")>]
    [<TestCase(50, Result="L")>]
    [<TestCase(100, Result="C")>]
    [<TestCase(500, Result="D")>]
    [<TestCase(1000, Result="M")>]
    member x.should_print_correct_character_when_input_is_symbol_number input=
        ToRoman input

    [<TestCase(4, Result="IV")>]
    [<TestCase(40, Result="XL")>]
    [<TestCase(400, Result="CD")>]
    member x.should_print_one_and_five_symbol_when_digit_is_four input=
        ToRoman input

    [<TestCase(9, Result="IX")>]
    [<TestCase(90, Result="XC")>]
    [<TestCase(900, Result="CM")>]
    member x.should_print_one_and_ten_symbol_when_digit_is_nine input=
        ToRoman input

    [<TestCase(1, Result="I")>]
    [<TestCase(2, Result="II")>]
    [<TestCase(30, Result="XXX")>]
    [<TestCase(200, Result="CC")>]
    member x.should_print_as_many_one_as_digit_when_digit_is_less_than_three input=
        ToRoman input

    [<TestCase(6, Result="VI")>]
    [<TestCase(7, Result="VII")>]
    [<TestCase(80, Result="LXXX")>]
    [<TestCase(700, Result="DCC")>]
    member x.should_print_five_and_as_many_one_as_digit_minus_five_when_digit_is_between_five_and_height input=
        ToRoman input

    [<TestCase(35, Result="XXXV")>]
    [<TestCase(178, Result="CLXXVIII")>]
    [<TestCase(2746, Result="MMDCCXLVI")>]
    member x.should_concatenate_results_when_many_digits input=
        ToRoman input

[<TestFixture>]
type RomanNumeralToArabic() = 
    [<TestCase("", Result=0)>]
    [<TestCase("I", Result=1)>]
    [<TestCase("V", Result=5)>]
    [<TestCase("X", Result=10)>]
    [<TestCase("L", Result=50)>]
    [<TestCase("C", Result=100)>]
    [<TestCase("D", Result=500)>]
    [<TestCase("M", Result=1000)>]
    member x.should_print_correct_number_when_input_is_specific_symbol input=
        ToArabic input

    [<TestCase("IV", Result=4)>]
    [<TestCase("XL", Result=40)>]
    [<TestCase("CD", Result=400)>]
    member x.should_print_four_when_input_is_one_and_five_symbol input=
        ToArabic input

    [<TestCase("IX", Result=9)>]
    [<TestCase("XC", Result=90)>]
    [<TestCase("CM", Result=900)>]
    member x.should_print_nine_when_input_is_one_and_ten_symbol input=
        ToArabic input

    [<TestCase("I", Result=1)>]
    [<TestCase("II", Result=2)>]
    [<TestCase("XXX", Result=30)>]
    [<TestCase("CC", Result=200)>]
    member x.should_print_count_of_one_when_symbols_are_only_one input=
        ToArabic input

    [<TestCase("VI", Result=6)>]
    [<TestCase("VII", Result=7)>]
    [<TestCase("LXXX", Result=80)>]
    [<TestCase("DCC", Result=700)>]
    member x.should_print_five_plus_count_of_one_when_symbols_are_five_followed_by_ones input=
        ToArabic input

    [<TestCase("XXXV", Result=35)>]
    [<TestCase("CLXXVIII", Result=178)>]
    [<TestCase("MMDCCXLVI", Result=2746)>]
    member x.should_concatenate_results_when_many_digits input=
        ToArabic input
