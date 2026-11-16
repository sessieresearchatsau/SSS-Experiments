ClearAll[decimalExpansionData, digitsString, finiteRepeatData, 
  icDigitParts, icBlockDigits, IC, ICNumber, ICDecimal, RepeatingDecimal, 
  icNeedsParensQ, parenthesizedBoxes, ICatNumeric, 
  finiteRepeatFraction, extendRepeat];

(*Find the decimal digits and the start of any infinite period.*)
decimalExpansionData[r : (_Integer | _Rational)] := 
  Module[{num = Abs[Numerator[r]], den = Denominator[r], intPart, rem,
     digits = {}, seen = <||>, prefixLength, periodLength}, 
   intPart = Quotient[num, den];
   rem = Mod[num, den];
   While[rem != 0 && ! KeyExistsQ[seen, rem], 
    seen[rem] = Length[digits];
    AppendTo[digits, Quotient[10 rem, den]];
    rem = Mod[10 rem, den];];
   prefixLength = If[rem == 0, Length[digits], seen[rem]];
   periodLength = Length[digits] - prefixLength;
   <|"Rational" -> r, "Sign" -> If[r < 0, -1, 1], 
    "IntegerPart" -> intPart, "Digits" -> digits, 
    "NonrepeatingLength" -> prefixLength, 
    "RepeatingLength" -> periodLength, 
    "NonrepeatingPart" -> Take[digits, prefixLength], 
    "RepeatingPart" -> Drop[digits, prefixLength]|>];

digitsString[digits_List] := StringJoin[ToString /@ digits];

(*Find the repeated run covering the most digits.\
Ties favor the earliest start,then the shortest block.*)
finiteRepeatData[digits_String] := 
  Module[{len = StringLength[digits], best = Missing["NoRepeat"], 
    bestLength = 0, start, width, block, count, runLength}, 
   Do[block = StringTake[digits, {start, start + width - 1}];
    count = 1;
    While[
     start + (count + 1) width - 1 <= len && 
      StringTake[
        digits, {start + count width, 
         start + (count + 1) width - 1}] === block, count++];
    runLength = count width;
    If[count >= 2 && runLength > bestLength, bestLength = runLength;
     best = <|"Prefix" -> StringTake[digits, start - 1], 
       "Block" -> block, "Count" -> count, 
       "Suffix" -> 
        StringDrop[digits, start + runLength - 1]|>;], {start, 1, 
     len - 1}, {width, 1, Quotient[len - start + 1, 2]}];
   best];

(*Search both sides of each run and preserve literal digits.*)
icDigitParts[""] := {};

icBlockDigits[pieces_List] := StringJoin[pieces /.
  IC[inner__, copies_Integer?Positive] :> StringRepeat[icBlockDigits[{inner}], copies]];

icDigitParts[digits_String] := 
  Module[{run = finiteRepeatData[digits]}, 
   If[AssociationQ[run], 
    Join[icDigitParts[run["Prefix"]], {IC[Sequence @@ icDigitParts[run["Block"]], run["Count"]]},
      icDigitParts[run["Suffix"]]], {digits}]];

(*Display a single I-Cat.*)
icNeedsParensQ[expr_String] := StringLength[expr] > 1;
icNeedsParensQ[_] := True;

parenthesizedBoxes[expr_, form_] := 
  Module[{boxes}, 
   boxes = If[StringQ[expr], expr, MakeBoxes[expr, form]];
   If[icNeedsParensQ[expr], RowBox[{"(", boxes, ")"}], boxes]];

(*Finite repeat.*)
IC /: MakeBoxes[expr : IC[block_, count_Integer?Positive], 
   StandardForm] := 
  With[{boxes = 
     RowBox[{UnderoverscriptBox["€", "", ToString[count]], 
       parenthesizedBoxes[block, StandardForm]}]}, 
   InterpretationBox[boxes, expr]];

(* The last argument counts copies of all preceding pieces together. *)
IC /: MakeBoxes[expr : IC[first_, rest__, count_Integer?Positive],
    StandardForm] :=
  With[{body = RowBox[(If[StringQ[#], #, ToBoxes[#, StandardForm]] &) /@
        {first, rest}]},
    InterpretationBox[
      RowBox[{UnderoverscriptBox["€", "", ToString[count]],
        "(", body, ")"}], expr]];

(*Infinite repeat starting at the first decimal place,as in 2/9.*)
IC /: MakeBoxes[expr : IC[block_, count_], StandardForm] /; 
   count === Infinity := 
  With[{boxes = 
     RowBox[{UnderoverscriptBox["€", "", "\[Infinity]"], 
       parenthesizedBoxes[block, StandardForm]}]}, 
   InterpretationBox[boxes, expr]];

(*Indexed repeat after a prefix,as in 11/90.*)
IC /: MakeBoxes[expr : IC[block_, {idx_, start_, stop_}], 
   StandardForm] := 
  With[{boxes = 
     RowBox[{UnderoverscriptBox["€", 
        RowBox[{MakeBoxes[idx, StandardForm], "=", 
          MakeBoxes[start, StandardForm]}], 
        MakeBoxes[stop, StandardForm]], 
       parenthesizedBoxes[block, StandardForm]}]}, 
   InterpretationBox[boxes, expr]];

(*Display the entire number as one row of digits and I-Cats.*)
ICNumber /: 
  MakeBoxes[expr : ICNumber[sign_, parts_List], StandardForm] := 
  Module[{items, boxes}, 
   items = If[sign < 0, Prepend[parts, "-"], parts];
   boxes = (If[StringQ[#], #, ToBoxes[#, StandardForm]] &) /@ items;
   With[{display = RowBox[boxes]}, InterpretationBox[display, expr]]];

(*Overbar notation.*)
Format[RepeatingDecimal[sign_, intPart_, prefix_, repeat_], 
   StandardForm] := 
  Interpretation[
   Row[{If[sign < 0, "-", ""], intPart, ".", prefix, OverBar[repeat]}],
    RepeatingDecimal[sign, intPart, prefix, repeat]];

Options[ICatNumeric] = {"Notation" -> "Overbar"};

ICatNumeric[r : (_Integer | _Rational), OptionsPattern[]] := 
  Module[{data = decimalExpansionData[r], whole, prefix, period, 
    periodLength, prefixLength, parts, runs, primary, formatted, 
    tailSpec}, whole = IntegerString[data["IntegerPart"]];
   prefix = digitsString[data["NonrepeatingPart"]];
   period = digitsString[data["RepeatingPart"]];
   prefixLength = data["NonrepeatingLength"];
   periodLength = data["RepeatingLength"];
   parts = icDigitParts[whole];
   If[prefix != "" || period != "", 
    parts = Join[parts, {"."}, icDigitParts[prefix]];];
   If[periodLength > 0, 
    tailSpec = 
     If[prefixLength == 0, Infinity, {n, prefixLength + 1, Infinity}];
    AppendTo[parts, IC[period, tailSpec]];];
   runs = 
    Cases[parts, 
     IC[pieces__, spec_] :> <|"Block" -> icBlockDigits[{pieces}], 
       "Count" -> If[ListQ[spec], Last[spec], spec]|>];
   primary = 
    If[runs === {}, <|"Block" -> "", "Count" -> 0|>, 
     First[MaximalBy[runs, StringLength[#["Block"]] #["Count"] &]]];
   formatted = 
    Which[OptionValue["Notation"] === "IC", 
     ICNumber[data["Sign"], parts], periodLength > 0, 
     RepeatingDecimal[data["Sign"], data["IntegerPart"], prefix, 
      period], IntegerQ[r], r, True, 
     Row[{If[r < 0, "-", ""], whole, ".", prefix}]];
   Join[data, <|"RepeatBlock" -> primary["Block"], 
     "BlockLength" -> StringLength[primary["Block"]], 
     "RepeatCount" -> primary["Count"], 
     "RepeatedDigitCount" -> 
      StringLength[primary["Block"]] primary["Count"], 
     "RepeatRuns" -> runs, "Formatted" -> formatted|>]];

(*Exact fraction for a block repeated a finite number of times.*)
finiteRepeatFraction[block_String, count_Integer?Positive] := 
  Module[{digits = StringRepeat[block, count]}, 
   FromDigits[digits]/10^StringLength[digits]];

(*Exact fraction for a decimal prefix followed by an infinite repeat.\
For example,extendRepeat["12","27"] gives 27/220.*)
extendRepeat[prefix_String, block_String] /; 
   StringMatchQ[prefix, RegularExpression["[0-9]*"]] && 
    StringMatchQ[block, RegularExpression["[0-9]+"]] := 
  Module[{prefixValue, prefixScale, blockDenominator}, 
   prefixValue = If[prefix === "", 0, FromDigits[prefix]];
   prefixScale = 10^StringLength[prefix];
   blockDenominator = 10^StringLength[block] - 1;
   (prefixValue + FromDigits[block]/blockDenominator)/prefixScale];
