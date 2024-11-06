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
    call void @printInt(i32 %_reg_0)
    %_reg_1 = call i32 @f(i32 %_reg_0)
    call void @printInt(i32 %_reg_1)
    call void @printInt(i32 %_reg_0)
    ret i32 0
}

define i32 @f(i32 %_reg_x) {
entry:
    %_reg_0 = mul i32 %_reg_x, %_reg_x
    ret i32 %_reg_0
}
