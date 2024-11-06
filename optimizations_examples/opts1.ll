declare void @printInt(i32)
declare i32 @readInt()
declare void @printString(i8*)
declare i8* @readString()
declare void @error()
declare i8* @_concatenateStrings(i8*, i8*)
declare i1 @_stringsEqual(i8*, i8*)
declare i1 @_stringsNotEqual(i8*, i8*)



define i32 @main() {
entry:
    %_reg_0 = call i32 @readInt()
    %_reg_1 = add i32 %_reg_0, 1
    call void @printInt(i32 %_reg_1)
    call void @printInt(i32 %_reg_1)
    %_reg_2 = call i32 @readInt()
    %_reg_3 = add i32 %_reg_2, 1
    call void @printInt(i32 %_reg_3)
    call void @printInt(i32 %_reg_1)
    ret i32 0
}
