      program FITer

      ! Definiciones
      real dx,ab(50),s,x(100),sum,a(20),T(50),p(10),e(9),abb(50),Rc(50),
     #Ro(50),suma,rms_anterior,rms3(3),ee(9),f(3),mar,pp(10),fijar_p(10)
     #,fijar_e(9),in_p,in_e
      integer n,k,npd,npf,j,h,npfm1,nc,ncm1,ii,jj,kk,ll,mm,ite,oo,qq,oma
     #x,lll,iii,jjj

      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! El programa modifica el modelo de suelo de n capas
      ! (n resisitvidades y n-1 espesore) que se le da como entrada
      ! en "Modelo_semilla.dat" para llegar a un modelo similar pero
      ! que tenga una respuesta teorica (Rc calculada por filtro inverso
      ! ONeill) lo mas parecida posible a los datos observados (Ro)
      ! que se le dan como entrada en "Ro.dat".
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Parametros para modificar procesamiento:
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------

      nc=5 !Numero de capas del modelo semilla (max=10)

      ra=0.5  !rango inicial de busqueda (como porsentaje de cantidad ajustada p o e)(0.5 busca en +- 50% del valor semilla en la primer iteracion)
      fra=0.9 !determina el acortamiento de radio de busqueda en cada iteracion. 0.999 es acortamiento lento y 0.001 acortamiento rapido

      ite=60  !en cada iteracion "ite" se ajustan una vez cada variable p y e, acortando el rango de busqueda segun los parametros "ra" y "fra".
      omax=10  !iteraciones globales (cada iteracion repite todas las iteraciones "ite" volviendo a usar el parametro "ra" inicial)

      !Fijar resisitvidades: no se modificara p(i) del modelo semilla si fijar_p(i)=1. :
      !            p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
      data fijar_p/0.,0.,0.,0.,1.,0.,0.,0.,0.,0./

      !Fijar resisitvidades: no se modificara e(i) del modelo semilla si fijar_e(i)=1. :
      !            e1 e2 e3 e4 e5 e6 e7 e8 e9
      data fijar_e/0.,0.,0.,1.,0.,0.,0.,0.,0./
      
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------

      !Controlar nc <= 10
      if (10.5 .lt. nc) then
      write(*,*)"ERROR: numero de capas(nc) debe ser menor o igual a 10"
      endif

      do qq=1,10    !aseguro vector de resistividades = 0.
      p(qq)=0.
      pp(qq)=0.
      enddo

      do qq=1,10    !aseguro vector de espesores = 0.
      e(qq)=0.
      ee(qq)=0.
      enddo




      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Definiciones para el filtro inverso O'Neill
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      !defino los coeficientes del filtro
      !(abscisas donde se quiere el valor de la resistividad aparente):
      data a/0.003042,-0.001198,0.01284,0.02350,0.08688,0.2374
     #,0.6194,1.1817,0.4248,-3.4507,2.7044,-1.1324,0.3930,-0.1436,0.0581
     #2,-0.02521,0.01125,-0.004978,0.002072,-0.000318/
      !corrimiento,n§ puntos por d‚cada, puntos que necesita el filtro
      !espaciamiento:
      s=-0.13069
      npd=6
      npf=20
      dx=log(10.)/npd
      !Entrada de absisas "Abs.dat":
      open(10,file="Abs.dat")
       n=0
33     n=n+1
       read(10,*,end=44) ab(n)
       go to 33
44     n=n-1
      close(10)
      



      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Entrada del modelo semilla "Modelo_semilla.dat" que se ajustara
      ! a los datos observados Ro:
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------

      do qq=1,10    !aseguro vector de resistividades = 0.
      p(qq)=0.
      pp(qq)=0.
      enddo

      do qq=1,10    !aseguro vector de espesores = 0.
      e(qq)=0.
      ee(qq)=0.
      enddo


      ncm1=nc-1

      open(22,file="Modelo_semilla.dat")   !Entrada de modelo semilla
      
       do i=1,nc
        read(22,*) p(i)  !Resistividad i
        pp(i)=p(i)
       enddo
       
       do i=1,nc-1
        read(22,*) e(i)   !Espesor i
        ee(i)=e(i)
       enddo
       
      close(22)
      

      !Se mustra en terminal el modelo de entrada (modelo semilla):

      write(*,*)"------------------------------------------------------"
      write(*,*)"Modelo Semilla (Entrada):"
      write(*,*)
      write(*,*)"p1 ---------------------- ",p(1)
      if (nc .lt. 1.5) then
      go to 22
      endif
      write(*,*)"p2 ---------------------- ",p(2)
      if (nc .lt. 2.5) then
      go to 22
      endif
      write(*,*)"p3 ---------------------- ",p(3)
      if (nc .lt. 3.5) then
      go to 22
      endif
      write(*,*)"p4 ---------------------- ",p(4)
      if (nc .lt. 4.5) then
      go to 22
      endif
      write(*,*)"p5 ---------------------- ",p(5)
      if (nc .lt. 5.5) then
      go to 22
      endif
      write(*,*)"p6 ---------------------- ",p(6)
      if (nc .lt. 5.5) then
      go to 22
      endif
      write(*,*)"p7 ---------------------- ",p(7)
      if (nc .lt. 5.5) then
      go to 22
      endif
      write(*,*)"p8 ---------------------- ",p(8)
      if (nc .lt. 5.5) then
      go to 22
      endif
      write(*,*)"p9 ---------------------- ",p(9)
      if (nc .lt. 5.5) then
      go to 22
      endif
      write(*,*)"p10 ---------------------- ",p(10)
      if (nc .lt. 5.5) then
      go to 22
      endif
       
       
22    write(*,*)
      write(*,*)"e1 ---------------------- ",e(1)
      if (nc .lt. 2.5) then
      go to 23
      endif
      write(*,*)"e2 ---------------------- ",e(2)
      if (nc .lt. 3.5) then
      go to 23
      endif
      write(*,*)"e3 ---------------------- ",e(3)
      if (nc .lt. 4.5) then
      go to 23
      endif
      write(*,*)"e4 ---------------------- ",e(4)
      if (nc .lt. 5.5) then
      go to 23
      endif
      write(*,*)"e5 ---------------------- ",e(5)
      if (nc .lt. 6.5) then
      go to 23
      endif
      write(*,*)"e6 ---------------------- ",e(6)
      if (nc .lt. 7.5) then
      go to 23
      endif
      write(*,*)"e7 ---------------------- ",e(7)
      if (nc .lt. 8.5) then
      go to 23
      endif
      write(*,*)"e8 ---------------------- ",e(8)
      if (nc .lt. 9.5) then
      go to 23
      endif
      write(*,*)"e9 ---------------------- ",e(9)
      if (nc .lt. 10.5) then
      go to 23
      endif



23      write(*,*)
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Entrada de los datos observados Ro:
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------

      open(30,file="Ro.dat")

        ii=0
66      ii=ii+1
        read(30,*,end=77) abb(ii),Ro(ii)
        go to 66
77    close(30)
        ii=ii-1
        

      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Procesamiento:
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------

      rms_anterior=10000.   !Para controlar errores (se pide rms < rms_anterior en cada iteracion)

      !Intercambia el orden en que se ajustan las cantidades p y e
      do iii=1,2             !0
           in_e=0.+(iii-1)
      do jjj=1,2             !00
           in_p=0.+(jjj-1)

      do oo=1,omax  !44


      !Valor inicial para factor de busqueda. En la primer iteracion "ite"
      !se compararan tres valores de cada variable y se elijira el que
      !arroje menor RMS. Los valores comparados seran:
      !cantidad*f(1) , cantidad*f(2) , cantidad*f(3).
      f(1)=1.0+ra
      f(2)=1.0
      f(3)=1.0-ra

      do mm=1,ite  !3    iteraciones "ite"

      do lll=1,nc-1  !2

       if (0.5.lt.in_e) then !invertir orden de pruebas
       ll=(nc-1)+1-lll
       else
       ll=lll
       endif


       if (0.5.lt.fijar_e(ll)) then   !fijar espesores fijados
       go to 299
       endif


      ! Se crean los 3 valores de la variable "ll" a comparar:
      do kk=1,3    !1

      e(ll)=ee(ll)*f(kk)


      ! Se calcula la respuesta teorica "Rc" que tendria el modelo con
      ! cada uno de los 3 valores de la variable "ll" a comparar:
      
      !siendo n abscisas efect£o la siguiente iteraci¢n
      do k=1,n
      !calculo la ubicaci¢n de puntos centrado en la abscisa
        h=0
        do j=1,npf
            h=h+1
            x(h)=exp(log(ab(k))+s+(j-15)*dx)
        enddo

      !calculo la transformada de resistividad aparente en el vector x
        call TRS(x,T,h,nc,p,e)
      !calculo la convoluci¢n entre el filtro y la transformada de resist
      !que me da la curva de resistividad aparente
        sum=0.
        npfm1=npf+1

        do h=1,npf
          sum=a(npfm1-h)*T(h)+sum
        enddo

        Rc(k)=sum
      enddo

      
      !calculo RMS para comparar (RMS de las diferencias entre Rc y Ro)
         suma=0.
        do jj=1,ii
           aux1=(Ro(jj)-Rc(jj))/Ro(jj)
           suma=aux1*aux1+suma
        enddo
        rms=sqrt(suma/ii)*100.
      
      rms3(kk)=rms
      
      enddo !1 kk
      

      ! Eleccion entre los 3 posibles valores de la variable "ll".
      ! (se elije el de menor RMS asociado)
       
       if ( (rms3(1).lt.rms3(2)).and. (rms3(1).lt.rms3(3)) ) then
              ee(ll)=ee(ll)*f(1)
              e(ll)=ee(ll)
              rms=rms3(1)

       else if ( (rms3(2).lt.rms3(1)).and. (rms3(2).lt.rms3(3)) ) then
              ee(ll)=ee(ll)*f(2)
              e(ll)=ee(ll)
              rms=rms3(2)

       else if ( (rms3(3).lt.rms3(1)).and. (rms3(3).lt.rms3(2)) ) then
              ee(ll)=ee(ll)*f(3)
              e(ll)=ee(ll)
              rms=rms3(3)
       endif
       
      ! A partir de ahora la variable "ll" del modelo semilla se remplaza
      ! por la elejida y se usara este nuevo modelo resultante para la
      ! siguiente iteracion.


      ! Se controla que RMS disminuya en cada iteracion (rms_anterior > rms)
      if ( (rms_anterior*1.01.lt.rms)) then
      write(*,*)"------------------------------------------------------"
      write(*,*)"ERROR: rms anterior<rms actual",mm,ll,rms_anterior,rms
      write(*,*)"------------------------------------------------------"
      pause
      else
      rms_anterior=rms
      endif
      


299   enddo !2 ll
      
      !Se define nuevo factor de busqueda (se reduce "ra").
      f(1)=1.+ra*(fra**mm)
      f(2)=1.0
      f(3)=1.-ra*(fra**mm)

      enddo !3 mm
      
      
      
      !-----------------------------------------------------------------
      !Hasta aca dentro de la iteracion "ite" se modifico (o no) una vez cada espesor.
      !Ahora se repite el procedimiento para resistividades en vez de espesores.
      !-----------------------------------------------------------------
      
      
      f(1)=1.0+ra    !Se vuelve al factor de busqueda inicial
      f(2)=1.0
      f(3)=1.0-ra


      do mm=1,ite  !33
      
      do lll=1,nc  !22

       if (0.5.lt.in_p) then
       ll=(nc-1)+1-lll     !invertir orden de pruebas
       else
       ll=lll
       endif


       if (0.5.lt.fijar_p(ll)) then      !fijar resistividades fijadas
       go to 2299
       endif


      do kk=1,3    !11


      p(ll)=pp(ll)*f(kk)



      !siendo n abscisas efect£o la siguiente iteraci¢n
      do k=1,n
      !calculo la ubicaci¢n de puntos centrado en la abscisa
        h=0
        do j=1,npf
            h=h+1
            x(h)=exp(log(ab(k))+s+(j-15)*dx)
        enddo

      !calculo la transformada de resistividad aparente en el vector x
        call TRS(x,T,h,nc,p,e)
      !calculo la convoluci¢n entre el filtro y la transformada de resist
      !que me da la curva de resistividad aparente
        sum=0.
        npfm1=npf+1

        do h=1,npf
          sum=a(npfm1-h)*T(h)+sum
        enddo

        Rc(k)=sum
      enddo


      !calculo RMS para comparar (RMS de las diferencias entre Rc y Ro)
         suma=0.
        do jj=1,ii
           aux1=(Ro(jj)-Rc(jj))/Ro(jj)
           suma=aux1*aux1+suma
        enddo
        rms=sqrt(suma/ii)*100.


      rms3(kk)=rms

      enddo !11 kk

      ! Eleccion entre los 3 posibles valores de la variable "ll".
      ! (se elije el de menor RMS asociado):
      
       if ( (rms3(1).lt.rms3(2)).and. (rms3(1).lt.rms3(3)) ) then
              pp(ll)=pp(ll)*f(1)
              p(ll)=pp(ll)
              rms=rms3(1)

       else if ( (rms3(2).lt.rms3(1)).and. (rms3(2).lt.rms3(3)) ) then
              pp(ll)=pp(ll)*f(2)
              p(ll)=pp(ll)
              rms=rms3(2)

       else if ( (rms3(3).lt.rms3(1)).and. (rms3(3).lt.rms3(2)) ) then
              pp(ll)=pp(ll)*f(3)
              p(ll)=pp(ll)
              rms=rms3(3)
       endif

      ! A partir de ahora la variable "ll" del modelo semilla se remplaza
      ! por la elejida y se usara este nuevo modelo resultante para la
      ! siguiente iteracion.


      ! Se controla que RMS disminuya en cada iteracion (rms_anterior > rms)
      if ( (rms_anterior*1.01.lt.rms)) then
      write(*,*)"------------------------------------------------------"
      write(*,*)"ERROR: rms anterior<rms actual",mm,ll,rms_anterior,rms
      write(*,*)"------------------------------------------------------"
      pause
      else
      rms_anterior=rms
      endif


2299  enddo !22 ll
      

      f(1)=1.+ra*(fra**mm)
      f(2)=1.0
      f(3)=1.-ra*(fra**mm)

      enddo !33 mm

99      enddo !44

      enddo  !00
      enddo  !0
      

      !Se muestra en terminal el modelo de salida

      write(*,*)"------------------------------------------------------"
      write(*,*)
      write(*,*)"Modelo Ajustado (Salida):"
      write(*,*)


      write(*,*)"p1 ---------------------- ",p(1)
      if (nc .lt. 1.5) then
      go to 24
      endif
      write(*,*)"p2 ---------------------- ",p(2)
      if (nc .lt. 2.5) then
      go to 24
      endif
      write(*,*)"p3 ---------------------- ",p(3)
      if (nc .lt. 3.5) then
      go to 24
      endif
      write(*,*)"p4 ---------------------- ",p(4)
      if (nc .lt. 4.5) then
      go to 24
      endif
      write(*,*)"p5 ---------------------- ",p(5)
      if (nc .lt. 5.5) then
      go to 24
      endif
      write(*,*)"p6 ---------------------- ",p(6)
      if (nc .lt. 5.5) then
      go to 24
      endif
      write(*,*)"p7 ---------------------- ",p(7)
      if (nc .lt. 5.5) then
      go to 24
      endif
      write(*,*)"p8 ---------------------- ",p(8)
      if (nc .lt. 5.5) then
      go to 24
      endif
      write(*,*)"p9 ---------------------- ",p(9)
      if (nc .lt. 5.5) then
      go to 24
      endif
      write(*,*)"p10 ---------------------- ",p(10)
      if (nc .lt. 5.5) then
      go to 24
      endif


24    write(*,*)
      write(*,*)"e1 ---------------------- ",e(1)
      if (nc .lt. 2.5) then
      go to 25
      endif
      write(*,*)"e2 ---------------------- ",e(2)
      if (nc .lt. 3.5) then
      go to 25
      endif
      write(*,*)"e3 ---------------------- ",e(3)
      if (nc .lt. 4.5) then
      go to 25
      endif
      write(*,*)"e4 ---------------------- ",e(4)
      if (nc .lt. 5.5) then
      go to 25
      endif
      write(*,*)"e5 ---------------------- ",e(5)
      if (nc .lt. 6.5) then
      go to 25
      endif
      write(*,*)"e6 ---------------------- ",e(6)
      if (nc .lt. 7.5) then
      go to 25
      endif
      write(*,*)"e7 ---------------------- ",e(7)
      if (nc .lt. 8.5) then
      go to 25
      endif
      write(*,*)"e8 ---------------------- ",e(8)
      if (nc .lt. 9.5) then
      go to 25
      endif
      write(*,*)"e9 ---------------------- ",e(9)
      if (nc .lt. 10.5) then
      go to 25
      endif
25    write(*,*)
      write(*,*)"RMS :",rms
      write(*,*)
      write(*,*)"------------------------------------------------------"


      !
      if (5 .lt. rms) then
      write(*,*)
      write(*,*)"ERROR:"
      write(*,*)
      write(*,*)"RMS demasiado alto."
      write(*,*)
      write(*,*)"El modelo de entrada es muy distinto del suelo real o"
      write(*,*)"los datos de Ro muy contaminados o ruidosos."
      write(*,*)"Probar otro modelo de entrada o suavisar datos Ro."
      write(*,*)"------------------------------------------------------"
      endif

      pause
      
      end   !end program FITer
      
      
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      ! Subrutina TRS
      !-----------------------------------------------------------------
      !-----------------------------------------------------------------
      
        Subroutine TRS(x,T,h,nc,p,e)
        real p(10),e(9),x(100),L,M,aux1,aux2,aux3,T(30)
        integer i,nc,ncm1,h,j
        data L,M,aux1,aux2,aux3/5*0./
        ncm1=nc-1
      !calculo las transformada de resistividad con el algoritmo de Sunde
        do j=1,h
           L=(p(nc)-p(nc-1))/(p(nc)+p(nc-1))
           aux1=L*exp(-2/x(j)*e(nc-1))
           M=(1.+aux1)/(1-aux1)
           if(ncm1.lt.2) go to 55
           do i=2,ncm1
              aux2=p(nc-i+1)*M
              L=(aux2-p(nc-i))/(aux2+p(nc-i))
              aux3=L*exp(-2./x(j)*e(nc-i))
              M=(1.+aux3)/(1.-aux3)
           enddo
55         T(j)=p(1)*M
        enddo
        end
