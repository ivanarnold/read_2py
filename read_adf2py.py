# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import adf04_2py as adf
import numpy as np

itieactn = 1

iunit = 19 ; titled = '---' ; iz = 18 ; iz0 = 0 ; iz1 = 0 ; il = 0
bwno = 0.0 ; npl = 0 ; bwnoa = 1.0 ; prtwta = 0.0
qdorb = 0.0 ; qdn =0.0 ; iorb = 0 ; ia = 0 ; cstrga = '-'
isa = 0 ; ila = 0 ; xja = 0.0 ; wa = 0.0 ; cpla = '-' ; npla = 0 ; ipla = 0
zpla = 0 ; nv = 0 ; scef = 0.0 ; itran = 0 ; maxlev = 0 ; tcode = '-'
ila = 0 ; i2a = 0 ; aval = 0.0 ; scom = 0.0 ; beth = 0 ; iadftyp = 0

cprta=np.asarray([['---------'],['---------']], dtype='c')

lbseta = False ; lqdorb = False ; lprn = False ; lcpl= False ; lorb = False
lbeth = False ; letyp = False ; lptyp = False ; lrtyp = False ; lhtyp = False
lityp = False ; lstyp = False ; lltyp = False ; ltied = False


adf04_dat = adf.xxdata_04(iunit, titled, iz, iz0, iz1 , bwno, npl, bwnoa ,
                          lbseta , prtwta , cprta , il , qdorb , lqdorb ,
                          qdn , iorb , ia , cstrga , isa , ila , xja , wa ,
                          cpla , npla, ipla , zpla , nv , scef , itran , 
                          maxlev , tcode , ila , i2a , aval , scom , beth ,
                          iadftyp , lprn, lcpl , lorb , lbeth , letyp, lptyp ,
                          lrtyp , lhtyp , lityp , lstyp , lltyp , itieactn ,
                          ltied )
