libname sasdata "C:\Temp" compress=no; 
   
  data sasdata.RImportTest ; 
   
  do i=1 to 10 ; 
    x=i ; 
    y=i*i ; 
    if i>5 then y=. ; 
    Weightincrease=y-x ; 
    Weightincreasedate=mdy(11,i,2007) ; 
    Dayasnumber =Weightincreasedate; 
    output ; 
  end; 
   
  label x= 'x has a short description'
        y= 'A very long description that is clearly longer than allowed'; 
  format y 8.1 Weightincreasedate date7. Dayasnumber 8.0; 
  run;
