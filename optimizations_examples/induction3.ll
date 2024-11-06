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
    br label %_lab_0

_lab_1:
    %_reg_5 = add i32 %_reg_2, 7
    %_reg_6 = add i32 %_reg_1, 7
    %_reg_7 = add i32 %_reg_0, 1
    %_reg_8 = add i32 %_reg_3, 7
    %_reg_9 = add i32 %_reg_4, 7
    br label %_lab_0

_lab_0:
    %_reg_0 = phi i32 [ 1, %entry ], [ %_reg_7, %_lab_1 ]
    %_reg_1 = phi i32 [ 0, %entry ], [ %_reg_6, %_lab_1 ]
    %_reg_2 = phi i32 [ -4, %entry ], [ %_reg_5, %_lab_1 ]
    %_reg_3 = phi i32 [ 7, %entry ], [ %_reg_8, %_lab_1 ]
    %_reg_4 = phi i32 [ 10, %entry ], [ %_reg_9, %_lab_1 ]
    %_reg_10 = icmp slt i32 %_reg_0, 10
    br i1 %_reg_10, label %_lab_1, label %_lab_2

_lab_2:
    call void @printInt(i32 %_reg_1)
    call void @printInt(i32 %_reg_2)
    call void @printInt(i32 %_reg_3)
    call void @printInt(i32 %_reg_4)
    ret i32 0
}
