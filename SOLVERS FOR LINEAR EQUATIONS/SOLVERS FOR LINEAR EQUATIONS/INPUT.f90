!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
! File name: INPUT.f90
! Author:Li Xinyu        
! Version:1.0        
! Date:2020/9/29
! Description: This is the module of reading equations from .txt and transfer them to solvers
! Others:  
! Function List:  
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!    2.	Date: 2020/9/30
!        Author: Li Xinyu
!        Modification: Change the rule of reading to read new cards.
!**************************************************************************************************
MODULE INPUT
	IMPLICIT NONE
	INTEGER,PARAMETER :: fileID = 10
	CONTAINS
	
	subroutine readtxt(A,b,x,N,methods)
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/30
	! Coded by:	Li Xinyu 
	! Calls: 
	! Called By: SOLVERSFORLINEAREQUATIONS(main program)
	! Input: A,b,x,N,methods     
	! Output: A,b,x,N,methods
	! Return:  
	! Others:       
	!*************************************************
		integer i,j,N,methods !读取用循环变量
		!real(kind = 8) Na(1)
		real(kind = 8),allocatable :: A(:,:), b(:), x(:) !定义矩阵A，向量b和待求解向量x
		character test !测试用字符
		open (unit = fileID,file = 'fin.txt')
		
		read (fileID,*) test !读取#用作判定
		if (test == '#') then
			read (fileID,'(1I3)') N
			write (*,*) '矩阵行、列数：'
			write (*,*) N
		else
			write (*,*) '输入格式错误：'
		end if
		
		allocate(A(N,N)) !分配空间
		allocate(b(N))
		allocate(x(N))
		
		write (*,*) !空一行
		read (fileID,*) test!跳至下一行
		if (test == '#') then
			read (fileID,*) ((A(i,j),j=1,N),i=1,N) !读取A矩阵。这里A是正常写法，故按照先读j再读i的顺序(先控制行不变，读列)，与内存排布方式相反。如果为了加快速度，写矩阵的时候就要转置写法
			write (*,*) "系数矩阵"
			write (*,*) A
		else
			write (*,*) '输入格式错误：'
		end if
		
		write (*,*) !空一行
		read (fileID,*) test!跳至下一行
		if (test == '#') then
			read (fileID,*) b !读取b转置尾数向量
			write (*,*) "尾数矩阵："
			do i=1,N
				write (*,'(D15.3)') b(i)
			end do
		else
			write (*,*) '输入格式错误：'
		end if
		
		write (*,*) !空一行
		read (fileID,*) test!跳至下一行
		if (test == '#') then
			read (fileID,*) methods !读取解法
			write (*,*) "解法："
			if (methods == 1) then !选择解法，1=列主元高斯
				write (*,*) "列主元高斯"
			else if (methods == 2) then !选择解法，2=LU
				write (*,*) "LU"
			end if
			write (*,*) !空一行
		else
			write (*,*) '输入格式错误：'
		end if
		
		close (fileID) !及时关闭fin.txt
		!return
		!call Gausssolve(A,b,x,N)  !调用回带函数
	end subroutine readtxt
END MODULE INPUT	