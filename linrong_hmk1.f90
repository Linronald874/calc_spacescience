! JungeLin 20190307

program score_statistics
	implicit none

	type Student
		integer :: number
		character(len=10) :: name
		real :: math_score
		character(len=10) :: country
	end type Student

	type(Student), dimension(10) :: group_1

	group_1(1) = Student(1, 'Steven', 95, 'China')
	group_1(2) = Student(2, 'Tony', 83, 'UK')
	group_1(3) = Student(3, 'Lily', 91, 'Japan')
	group_1(4) = Student(4, 'Mike', 90,'USA')

 	print*, 'The original group is'
 	call print_group(group_1,4)
 	call sort_use_math(group_1,1,4)
 	print*, 'The sorted group is'
 	call print_group(group_1,4)
 	print*, 'The mean math score is'
 	write(*, fmt="('   ',F4.1)") mean_math_scr(group_1,4)
 	print*, 'The standard deviation of the math scores is'
 	write(*, fmt="('  ',F4.1)") std_err_math_scr(group_1,4)
 	 	 
	contains
	subroutine print_student(student_x)
		implicit none
		type(Student), intent(in) :: student_x
		write(*, fmt="(I3,'  ',A10,F4.1,'  ',A10)") student_x%number, student_x%name,&
			    student_x%math_score, student_x%country
	end subroutine print_student

	subroutine swap_student(student_a,student_b)
		implicit none
		type(Student), intent(inout) :: student_a, student_b
		type(Student) :: c
		c = student_a
		student_a = student_b
		student_b = c 
	end subroutine swap_student

	subroutine print_group(group,end)
		implicit none
		integer :: sz,i,end
		type(Student), intent(in), dimension(:) :: group

		if ( 0==end ) then
			sz = size(group)
		else
			sz = end
		end if

		print_each: do i = 1, sz
			call print_student(group(i))
		end do print_each
	end subroutine print_group

	recursive subroutine sort_use_math(group,left,right)
		implicit none
		type(Student), intent(inout), dimension(:) :: group
		integer :: left,right,i,j
		real :: current_value
		if ( left >= right ) then
			return
		end if

		i = left+1
		j = right
		current_value = group(left)%math_score

		do while(i<j)
			do while(i<j .and. group(j)%math_score>current_value)
				j = j-1
			end do
			do while(i<j .and. group(i)%math_score<current_value)
				i = i+1
			end do 
			if ( i<j ) then
				call swap_student(group(i),group(j))
			end if
		end do 
		call swap_student(group(i),group(left))

		call sort_use_math(group,left,i-1)
		call sort_use_math(group,i+1,right)
		
	end subroutine sort_use_math

	function mean_math_scr(group,end) result(mean)
		implicit none
		integer :: sz,i,end
		real :: sum,mean
		type(Student), intent(in), dimension(:) :: group

		if ( 0==end ) then
			sz = size(group)
		else
			sz = end
		end if

		sum = 0.0
		calcsum: do i = 1, sz
			sum = sum + group(i)%math_score
		end do calcsum
		mean = sum/sz
	end function mean_math_scr

	function std_err_math_scr(group,end)
		implicit none
		real :: std_err_math_scr, sqr_err, mean
		integer :: sz,i,end
		type(Student), intent(in), dimension(:) :: group

		if ( 0==end ) then
			sz = size(group)
		else
			sz = end
		end if

		mean = mean_math_scr(group,sz)
		sqr_err = 0.0
		do i = 1, sz
			sqr_err = sqr_err + (group(i)%math_score - mean)**2
		end do
		sqr_err = sqr_err / sz
		std_err_math_scr = sqrt(sqr_err)
	end function std_err_math_scr

end program score_statistics
