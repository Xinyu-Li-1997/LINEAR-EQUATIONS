!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
! File name: UPTRI.f90
! Author: Li Xinyu, Zhou Xingguang        
! Version: 1.0        
! Date: 2020/9/29
! Description: To calculate x in up triangle
! Others: None
! Function List: None
! Sunroutine List: Gausssolve
! Module list:  None
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!**************************************************************************************************
MODULE UPTRI
	IMPLICIT NONE
	CONTAINS
	subroutine UPTRIsover(A,b,x,N)
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/29
	! Coded by:	Li Xinyu 
	! Calls: None      
	! Called By: Gausssolve(A,b,x,N),LUsolve(A,b,x,N)    
	! Input: A,b,N      
	! Output: x
	! Return: x     
	! Others:       
	!*************************************************
		implicit none
		integer i,j,N !i行号，j列号，N矩阵行、列数
		real(kind = 8) :: A(N,N), b(N), x(N) !定义矩阵A，向量b和待求解向量x
		!下面开始回代
		x(N) = b(N)/A(N,N) !先算最后一行
		do i=N-1, 1, -1 !i做减法反循环
			x(i) = b(i)
			do j=i+1, N !一列列的减完
				x(i) = x(i) - x(j)*A(i,j)
			end do
			x(i) = x(i)/A(i,i) !最后除以当前行x(i)的系数
		end do
	end subroutine UPTRIsover
END MODULE UPTRI