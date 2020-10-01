!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
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
		
		open (unit = fileIDmake,file = 'fin.txt') !打开文件
		
		write (fileIDmake,*) "#请在下一行输入系数矩阵行、列数："
		write (fileIDmake,*) "#请在下一行输入系数矩阵，每行元素空格隔开(请勿转置)："
		write (fileIDmake,*) "#请在下一行输入尾数转置向量，每行元素空格隔开："
		write (fileIDmake,*) "#请在下一行选择解法，1=列主元高斯；2=LU分解法："
		
		close (fileIDmake)
		
	end PROGRAM
	