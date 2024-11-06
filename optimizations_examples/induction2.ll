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
    %_reg_1 = mul i32 10, %_reg_0
    br label %_lab_0

_lab_1:
    %_reg_4 = add i32 %_reg_3, 2
    %_reg_5 = add i32 %_reg_2, 20
    call void @printInt(i32 %_reg_5)
    br label %_lab_0

_lab_0:
    %_reg_2 = phi i32 [ %_reg_1, %entry ], [ %_reg_5, %_lab_1 ]
    %_reg_3 = phi i32 [ %_reg_0, %entry ], [ %_reg_4, %_lab_1 ]
    %_reg_6 = icmp slt i32 %_reg_3, 5
    br i1 %_reg_6, label %_lab_1, label %_lab_2

_lab_2:
    ret i32 0
}
