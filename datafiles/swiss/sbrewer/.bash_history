v.to.rast input="border" output="border" use="cat" type="area" layer=1
g.list
v.to.rast input="borders" output="borderr" use="cat" type="area" layer=1
g.remove borderr
v.to.rast input="borders" output="borderr" use="cat" type="area" layer=1
d.mon start=x0
d.rast borderr
d.rast dem
r.mask borderr
d.rast dem
g.remove MASK
d.rast dem
v.build.polylines input=borders output=bordpoly
d.vect bordpoly
v.to.rast input=bordpoly output=bordrast
v.to.rast input=bordpoly output=bordrast use=cat
d.rast bordrast
v.to.rast input=bordpoly output=bordrast use=val
d.rast bordrast
d.vect bordpoly
v.to.rast input=bordpoly output=bordrast use=1
v.to.rast input=bordpoly output=bordrast type=area
g.remove bordrast
v.to.rast input=bordpoly output=bordrast type=area
v.to.rast input=bordpoly output=bordrast type=area column=1
v.to.rast input=bordpoly output=bordrast type=area column=2
v.to.rast input=bordpoly output=bordrast type=area column=3
v.to.rast input=bordpoly output=bordrast type=area use=cat
d.rast bordrast
v.support bordpoly
g.remove bordrast
d.vect bordpoly
v.to.rast input=bordpoly output=bordrast type=area use=cat
d.rast bordrast
g.remove bordrast
v.info bordpoly
v.db.addcol bordpoly layer=1 columns=int
v.dbconnect 
v.db.connect 
d.vect bordpoly
v.to.rast input=bordpoly output=bordrast type=area use=cat
d.rast bordrast
v.category input=bordpoly output=bordnew option=add
d.vect bordpoly
v.to.rast input=bordpoly output=bordrast type=area use=cat
g.remove bordpoly
g.remove bordrast
v.to.rast input=bordpoly output=bordrast type=area use=cat
d.rast bordrast
v.to.rast input=bordpoly output=bordrast use=cat
g.remove bordrast
v.to.rast input=bordpoly output=bordrast use=cat
d.rast bordrast
v.to.rast
v.info borders
v.info bordpoly
v.info bordpoly
v.centroid
v.centroids 
v.info -t borders
v.info -t bordpoly
v.clean
v.info -t bordersnap
v.clean
v.clean input=bordpoly output=bord2 tool=break,snap thresh=10,10
v.clean input=bordpoly output=bord2 tool=break,snap thresh=1000,1000
v.clean input=bordpoly output=bord2 tool=break,snap thresh=1000,1000 -o
v.clean input=bordpoly output=bord2 tool=break,snap thresh=1000,1000 --o
v.clean input=bordpoly output=bord2 tool=break,snap thresh=1,1 --o
v.in.ogr
g.remove pp
g.remove bord2
g.remove bvect=ord2
g.remove vect=ord2
g.remove vect=bord2
g.remove vect=pp
g.list vect
g.remove vect=bordsnap
g.remove vect=bordnew
g.remove vect=bordpolysnap
touch makepoly.sh
ls
chmod +x makepoly.sh 
./makepoly.sh 
d.vect bord_clean_type_centroids
v.to.rast
ls
rm out\=- 
less temp.grd 
exit
cd Documents/Classes/GEOG6000/Lectures/18\ Spatial\ VI/
ls
less swiss_ppt.csv 
perl -pi -e 's/\r\n?/\n/g' *.csv
g.remove MASK
less swiss_ppt.csv 
cp swiss_ppt.csv temp.csv
vim temp.csv 
r.what dem < awk -F',' '{print $2, $3}' swiss_ppt.csv 
awk -F',' '{print $2, $3}' swiss_ppt.csv 
awk -F',' '{print $2, $3}' swiss_ppt.csv | r.what dem
awk -F',' '{print $2, $3}' swiss_ppt.csv | r.what dem |sed 's/|/,/g'
awk -F',' '{print $2, $3, $4}' swiss_ppt.csv | r.what dem |sed 's/|/,/g'
awk -F',' '{print $2, $3, $4}' swiss_ppt.csv | r.what dem |sed 's/|/,/g' > swiss_elev.csv
awk -F',' '{print $2, $3, $4}' swiss_ppt.csv | r.what dem |sed 's/|/,/g' > swiss_elev.csv
quit
exit
