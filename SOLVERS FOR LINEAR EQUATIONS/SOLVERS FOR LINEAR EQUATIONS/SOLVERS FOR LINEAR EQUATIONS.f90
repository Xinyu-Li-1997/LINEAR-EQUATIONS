!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
! File name: SOLVERS FOR LINEAR EQUATIONS.f90
! Author:Li Xinyu        
! Version:1.0        
! Date:2020/9/29
! Description: !!The main program!! To read equations, choose solvers, solve, and output results
! Others:  This program	is also an interface to	solutions for linear equations as much as possible
! Function List: None
! Sunroutine List: CHOOSE SOVERS
! Module list: None
!	INPUT('INPUT.f90'). Reading equations and transfer them to solvers.
!	GAUSSIAN COLUMN('SOVER GAUSSIAN ITER OF COLUMN.f90'). Sover of GAUSSIAN ITER OF COLUMN.
!	LU(SOVER LU.f90). Sover of LU
!	UPTRI(UPTRI.f90). To calculate x in	up triangle
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!    2. Date: 2020/9/30
!        Author: Li Xinyu
!        Modification: Developed the MCARDS module to make readable cards for users
!**************************************************************************************************
program SOLVERSFORLINEAREQUATIONS
	use INPUT !读卡模块
	use GAUSSIAN_COLUMN !列主元高斯迭代法模块
	use LU !LU分解模块
	use UPTRI !回代模块
	implicit none
	INTEGER,PARAMETER:: fileIDout = 20
	INTEGER :: N = 1 !维数初值	
	INTEGER :: methods
	
	real(kind = 8),allocatable :: A(:,:), b(:), x(:) !定义矩阵A，向量b和待求解向量x
	call readtxt(A,b,x,N,methods) !调用读取函数
	
	if (methods == 1) then !选择解法，1=列主元高斯
		call Gausssolve(A,b,x,N)  !启用列主元高斯法，自动启动回代计算
	else if (methods == 2) then !选择解法，2=LU
		call LUsolve(A,b,x,N)   !启用LU法，自动启动回代计算
	end if
	
	!下面开始输出
	write (*,*) "x的解为："
	write (*,'(D15.3)') x
	open (unit = fileIDout,file = 'fout.txt') !输出
	write (fileIDout,'(D15.3)') x
	close (fileIDout) !及时关闭fin.txt
	
	deallocate(A)
	deallocate(b)
	deallocate(x)
	
	pause
	
end program SOLVERSFORLINEAREQUATIONS
	

