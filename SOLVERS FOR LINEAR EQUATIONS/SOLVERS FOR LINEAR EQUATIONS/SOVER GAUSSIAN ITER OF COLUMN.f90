!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
! File name: SOVER GAUSSIAN ITER OF COLUMN.f90
! Author: Li Xinyu    
! Version: 1.0        
! Date: 2020/9/29
! Description: This is the sover of GAUSSIAN ITER OF COLUMN
! Others: None
! Function List: None
! Sunroutine List: Gausssolve
! Module list:  UPTRI
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!**************************************************************************************************
MODULE GAUSSIAN_COLUMN
	USE UPTRI
	IMPLICIT NONE
	
	!INTEGER,PARAMETER:: DBL = 8
	CONTAINS
	subroutine Gausssolve(A,b,x,N) 
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/29
	! Coded by:	Li Xinyu 
	! Calls: UPTRIsover(Aup,bup,x,N)      
	! Called By: readtxt(A,b,x,N)
	! Input: A,b,N      
	! Output:
	! Return:x     
	! Others:       
	!*************************************************
		implicit none
		integer :: i, k !循环变量，i行号，k列号
		integer :: N !矩阵行/列数
		integer :: idmax !主元素编号
		real(kind = 8) :: elmax !主元素绝对值 
		real(kind = 8) :: temp !暂存同列临行的系数比 
		real(kind = 8) :: A(N,N), b(N), x(N) !定义矩阵A，向量b和待求解向量x
		real(kind = 8) :: Aup(N,N), bup(N) 
		real(kind = 8) :: Ab(N,N+1) !增广矩阵
		real(kind = 8) :: vtemp1(N+1), vtemp2(N+1) !用作换行的中间数组
		
		Ab(1:N,1:N) = A !对增广Ab的系数区域赋值为A
		Ab(:,N+1) = b !对增广Ab的尾数区域赋值为b
		!===============================================================
		!列主元消去法核心
		do k=1, N-1 !k作为列编号，外循环扫描行
			elmax = dabs(Ab(k,k)) !记录数值
			idmax = k !记录行位置
			do i=k+1, N !先扫描列，i为行号
				if (dabs(Ab(i,k)) > elmax) then
					elmax = dabs(Ab(i,k)) !获取更大的值
					idmax = i
				end if
			end do !至此已完成了最大元素的查找，下面交换行顺序
			vtemp1 = Ab(k,:)
			vtemp2 = Ab(idmax ,:)
			Ab(k,:) = vtemp2
			Ab(idmax ,:) = vtemp1
			!开始消元
			do i = k+1, N
				temp = Ab(i,k)/Ab(k,k)
				Ab(i,:) = Ab(i,:) - temp*Ab(k,:) !k+1行根据k行消元
			end do
		end do 
		!至此，Ab已转换为上三角阵
		!===============================================================
		Aup(:,:) = Ab(1:N,1:N) !拆分增广阵为系数阵，储存
		bup(:) = Ab(:,N+1) !拆分增广阵为尾数阵，储存
		!write (*,*) "新系数矩阵"
		!write (*,*) Aup
		!write (*,*) "新尾数矩阵"
		!write (*,*) bup
		call UPTRIsover(Aup,bup,x,N) !调用回带函数
	end subroutine Gausssolve
END MODULE GAUSSIAN_COLUMN
	