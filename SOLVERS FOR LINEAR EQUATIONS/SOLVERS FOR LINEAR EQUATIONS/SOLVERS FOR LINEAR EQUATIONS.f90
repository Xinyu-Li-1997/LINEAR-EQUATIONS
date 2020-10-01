!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
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
	use INPUT !����ģ��
	use GAUSSIAN_COLUMN !����Ԫ��˹������ģ��
	use LU !LU�ֽ�ģ��
	use UPTRI !�ش�ģ��
	implicit none
	INTEGER,PARAMETER:: fileIDout = 20
	INTEGER :: N = 1 !ά����ֵ	
	INTEGER :: methods
	
	real(kind = 8),allocatable :: A(:,:), b(:), x(:) !�������A������b�ʹ��������x
	call readtxt(A,b,x,N,methods) !���ö�ȡ����
	
	if (methods == 1) then !ѡ��ⷨ��1=����Ԫ��˹
		call Gausssolve(A,b,x,N)  !��������Ԫ��˹�����Զ������ش�����
	else if (methods == 2) then !ѡ��ⷨ��2=LU
		call LUsolve(A,b,x,N)   !����LU�����Զ������ش�����
	end if
	
	!���濪ʼ���
	write (*,*) "x�Ľ�Ϊ��"
	write (*,'(D15.3)') x
	open (unit = fileIDout,file = 'fout.txt') !���
	write (fileIDout,'(D15.3)') x
	close (fileIDout) !��ʱ�ر�fin.txt
	
	deallocate(A)
	deallocate(b)
	deallocate(x)
	
	pause
	
end program SOLVERSFORLINEAREQUATIONS
	

