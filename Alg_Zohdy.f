        program Zhody
        real x(100),xv(100),RAo(100),RAc(100),RV(100),aux1,aux2,sum,rms
        real z(100)
        integer i,j
        parameter(tol=0.2)
        write(*,*)"____________________________________________________"
        write(*,*)"   Este programa calcula un corte de resistividad   "
        write(*,*)"   verdadera a partir de una curva de resistividad  "
        write(*,*)"   aparente aplicando el algoritmo de Zohdy(1989).  "
        write(*,*)"----------------------------------------------------"
        write(*,*)"      Necesita que se ingrese el archivo RA.dat.    "
        write(*,*)"    Este tendra los valores de la CRA, sin saltos,  "
        write(*,*)"por lo que previamente debera ajustarse cada empalme"
        write(*,*)"       El archivo debe constar de dos columnas.     "
        write(*,*)"          La primera con los valores de AB/2        "
        write(*,*)"       y la segunda con la resistividad aparente    "
        write(*,*)"----------------------------------------------------"
        write(*,*)"    El programa genera entonces el archivo RV.txt   "
        write(*,*)"          con el resultado del algoritmo.           "
        write(*,*)"____________________________________________________"
        write(*,*)
        pause
C     leo los valores de la resistividad aparente de entrada
        open(10,file="RA.dat")

        i=0
33      i=i+1
          read(10,*,end=44) xv(i),RAo(i)
        go to 33
44      close(10)
        i=i-1
C     considero que el corte geoel‚ctrico tiene tantas capas como puntos
C     muestreados y que las abscisas son las profundidades, los valores
C     de resistividad aparente ser n considerados como verdaderos
      do j=1,i
         RV(j)=RAo(j)
         z(j)=xv(j)
      enddo
C     calculo los espesores de cada capa
      x(1)=z(1)
      do j=2,i
         x(j)=z(j)-z(j-1)
      enddo
      aux2=1000.
55      call TRS(xv,x,RV,i,RAc)
C     calculo el error medio cuadr tico entre la curva inicial observada
C     y la calculada con el corte geoel‚ctrico considerado
        sum=0.
        do j=1,i
           aux1=(RAo(j)-RAc(j))/RAo(j)
           sum=aux1*aux1+sum
        enddo

        rms=sqrt(sum/i)*100.
C     se ajustan los valores de las abscisas hasta que el error no
C     disminuya mas
        if (rms.lt.aux2) then
           aux2=rms
           do j=1,i
              x(j)=0.9*x(j)
           enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     calculo las profundidades de cada capa y escribo el archivo con los
C     modelos propuestos
           z(1)=x(1)
           write(12,*) z(1),RAo(1)
           do j=2,i
              z(j)=x(j)+z(j-1)
           enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
           go to 55
        endif
C     hago el ajuste de los valores de "resistividad verdadera"
        aux2=2.*rms
66      call TRS(xv,x,RV,i,RAc)
        sum=0.
        do j=1,i
           aux1=(RAo(j)-RAc(j))/RAo(j)
           sum=aux1*aux1+sum
        enddo

        rms=sqrt(sum/i)*100.
        if ((rms.lt.aux2).and.(rms.gt.tol)) then
           aux2=rms
           do j=1,i
              RV(j)=RV(j)*RAo(j)/RAc(j)
           enddo
           go to 66
        endif
C     cuando el error no disminuye de una iteraci¢n a la siguiente
C     obtengo un corte geoel‚ctrico que ajusta los datos, tiene tantas
C     capas como abscisas
      open(11,file="RV.txt")
          write(11,*)"Prof  RV"
             write(11,*) 0.1,RV(1)
          do j=1,i-1
             write(11,*) z(j),RV(j)
             write(11,*) z(j),RV(j+1)
          enddo
          write(11,*) z(i),RV(i)
          write(11,*) 999.,RV(i)
          write(11,*)
          write(11,*)"Abs   RAo  RAc"
          do j=1,i
             write(11,*) xv(j),RAo(j),RAc(j)
          enddo

      close(11)
      write(*,*)
      write(*,*)"      ­LISTO!"
      write(*,*)
      write(*,*)"--------------------------------------------------"
      write(*,*)" Se obtuvo el siguiente error de ajuste entre la"
      write(*,*)" respuesta del modelo y la curva observada:"
      write(*,*)"--------------------------------------------------"
      write(*,*)
      write(*,78) "RMS=",rms,"%"
      pause
      write(*,*)
      write(*,*) "Archivo de salida RV.txt"
76    format(F7.1,5x,F7.1)
77    format(F7.0,F7.1,F7.1)
78    format(A,F10.2,A)
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Subroutine TRS(y,e,p,h,RAc)
      real p(100),e(100),y(100),L,M,aux1,aux2,aux3,T(100),s,dx,RAc(100),
     #sum,x(100),a(20)
      integer npf,npd,k,h1,h,hm1,j,i,npfm1
      data L,M,aux1,aux2,aux3,a/5*0.,0.003042,-0.001198,0.001284,0.02350
     #,0.08688,0.2374,0.6194,1.1817,0.4248,-3.4507,2.7044,-1.1324,0.3930
     #,-0.1436,0.05812,-0.02521,0.01125,-0.004978,0.002072,-0.000318/
C     el vector a contiene los coeficientes del filtro de O'Neill, s es
C     el corrimiento, npd es el numero de puntos por decada y npf es el
C     numero de puntos del filtro
      s=-0.13069
      npd=6
      npf=20
      dx=log(10.)/npd
      do k=1,h
C     calculo la ubicaci¢n de puntos centrado en la abscisa
        h1=0
        do j=1,npf
            h1=h1+1
            x(h1)=exp(log(y(k))+s+(j-15)*dx)
        enddo
        hm1=h-1
C     calculo las transformada de resistividad con el algoritmo de Sunde
        do j=1,h
           L=(p(h)-p(h-1))/(p(h)+p(h-1))
           aux1=L*exp(-2/x(j)*e(h-1))
           M=(1.+aux1)/(1-aux1)

           do i=2,hm1
              aux2=p(h-i+1)*M
              L=(aux2-p(h-i))/(aux2+p(h-i))
              aux3=L*exp(-2./x(j)*e(h-i))
              M=(1.+aux3)/(1.-aux3)
           enddo
           T(j)=p(1)*M
        enddo
C     calculo la convoluci¢n entre el filtro y la transformada de resist
C     que me da la curva de resistividad aparente
        sum=0.
        npfm1=npf+1
        do h1=1,npf
          sum=a(npfm1-h1)*T(h1)+sum
        enddo
        RAc(k)=sum
      enddo
      end
