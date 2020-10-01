!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
! File name: SOLVERS FOR LINEAR EQUATIONS.f90
! Author:Li Xinyu        
! Version:1.0        
! Date:2020/9/30
! Description: !!The main program!! To make cards and solve methods
! Function List: None
! Sunroutine List: None
! Module list: None
! History:  
!    1. Date: 2020/9/30
!        Author: Li Xinyu
!        Modification: Developed the program to make readable cards for users
!**************************************************************************************************	
	PROGRAM MAKETXT
		IMPLICIT NONE
		INTEGER,PARAMETER:: fileIDmake = 11
		
		open (unit = fileIDmake,file = 'fin.txt') !���ļ�
		
		write (fileIDmake,*) "#������һ������ϵ�������С�������"
		write (fileIDmake,*) "#������һ������ϵ������ÿ��Ԫ�ؿո����(����ת��)��"
		write (fileIDmake,*) "#������һ������β��ת��������ÿ��Ԫ�ؿո������"
		write (fileIDmake,*) "#������һ��ѡ��ⷨ��1=����Ԫ��˹��2=LU�ֽⷨ��"
		
		close (fileIDmake)
		
	end PROGRAM
	