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
    %_reg_1 = call i32 @readInt()
    %_reg_2 = icmp slt i32 %_reg_0, %_reg_1
    br i1 %_reg_2, label %_lab_0, label %_lab_1

_lab_0:
    br label %_lab_2

_lab_1:
    br label %_lab_2

_lab_2:
    %_reg_3 = phi i32 [ 1, %_lab_0 ], [ 0, %_lab_1 ]
    br i1 %_reg_2, label %_lab_3, label %_lab_4

_lab_3:
    %_reg_4 = add i32 %_reg_3, 1
    br label %_lab_5

_lab_4:
    br label %_lab_5

_lab_5:
    %_reg_5 = phi i32 [ %_reg_4, %_lab_3 ], [ %_reg_3, %_lab_4 ]
    call void @printInt(i32 %_reg_5)
    ret i32 0
}
