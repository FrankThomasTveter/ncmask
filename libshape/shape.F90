!    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>
!
!    This file is part of FortranGIS.
!
!    FortranGIS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation, either version 3 of the
!    License, or (at your option) any later version.
!
!    FortranGIS is distributed in the hope that it will be useful, but
!    WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public
!    License along with FortranGIS.  If not, see
!    <http://www.gnu.org/licenses/>.

!> Fortran 2003 interface to the libshape http://libshape.maptools.org/
!! library.
!! This module defines an API which reflects the original libshape C
!! API with some modifications:
!!
!!  - \a shpopen, \a shpcreate and \a shpclose functions act also on
!!    .dbf files
!!  - \a shpgetinfo accesses also .dbf information
!!  - the \a DBFRead*Attribute and \a DBWrite*Attribute are converted
!!    into two f90 interfaces called \a dbfreadattribute and \a
!!    dbwriteattribute respectively.
!!
!! The module defines two derived types: \a shpfileobject associated
!! to a shapefile dataset, and \a shpobject associated to a single
!! shape within a dataset. Access to database (.dbf) information is
!! done by accessing the file object only.
!!
!! For an example of application of the \a libshape module, please
!! refer to the following test program, which creates a shapefile and
!! successively reads it:
!! \include libshape_test.F90
!!
!! \ingroup libfortrangis
MODULE shape
  USE,INTRINSIC :: ISO_C_BINDING
  USE fortranc
  IMPLICIT NONE
  logical     :: shape_bdeb=.false.
  INTEGER,PARAMETER :: shpt_null = 0 !< Series of constants for specifying type of new shape datasets with \a shpcreate, null shape
  INTEGER,PARAMETER :: shpt_point = 1 !< points
  INTEGER,PARAMETER :: shpt_arc = 3 !< arcs (Polylines, possible in parts)
  INTEGER,PARAMETER :: shpt_polygon = 5 !< polygons (possible in parts)
  INTEGER,PARAMETER :: shpt_multipoint = 8 !< multiPoint (related points)
  INTEGER,PARAMETER :: shpt_pointz = 11 !< 3D (+ measure) points
  INTEGER,PARAMETER :: shpt_arcz = 13 !< 3D (+ measure) arcs
  INTEGER,PARAMETER :: shpt_polygonz = 15 !< 3D (+ measure) polygons
  INTEGER,PARAMETER :: shpt_multipointz = 18 !< 3D (+ measure) multiPoint
  INTEGER,PARAMETER :: shpt_pointm = 21 !< 2D + measure points
  INTEGER,PARAMETER :: shpt_arcm = 23 !< 2D + measure arcs
  INTEGER,PARAMETER :: shpt_polygonm = 25 !< 2D + measure polygons
  INTEGER,PARAMETER :: shpt_multipointm = 28 !< 2D + measure multiPoint

  INTEGER,PARAMETER :: shpt_multipatch = 31 !< complex (TIN-like) with Z, and Measure

  INTEGER,PARAMETER :: ftstring = 0 !< Series of constants for specifying dbf field type, fixed length string field
  INTEGER,PARAMETER :: ftinteger = 1 !< numeric field with no decimals
  INTEGER,PARAMETER :: ftdouble = 2 !< numeric field with decimals
  INTEGER,PARAMETER :: ftlogical = 3 !< LOGICAL field
  INTEGER,PARAMETER :: ftinvalid = 4 !< not a recognised field TYPE

  TYPE shpSeg
     real :: spos(3), epos(3)
     type(shpSeg), pointer :: prev => null()
     type(shpSeg), pointer :: next => null()
  END type shpSeg
  
  !> Object describing a shapefile dataset.
  !! Its components are private so they should not be manipulated
  !! directly.
  TYPE shpfileobject
     PRIVATE
     TYPE(c_ptr) :: shpfile_orig=c_null_ptr
     TYPE(c_ptr) :: dbffile_orig=c_null_ptr
  END TYPE shpfileobject


  !> Object describing the geometrical properties of a shape.
  !! It is used for reading a shape, or for manipulating a newly created
  !! shape; in the latter case the single values can be changed, but not
  !! the size of the arrays.
  TYPE shpobject
     TYPE(c_ptr) :: shpobject_orig=c_null_ptr !< pointer to C information, it should not be used
     INTEGER :: nshptype=0 !< shape type, one of the \a shpt_* constants defined
     INTEGER :: nshapeid=-1 !< shape number (-1 is unknown/unassigned)
     INTEGER :: nparts=0 !< number of parts (0 implies single part with no info)
     INTEGER,POINTER :: panpartstart(:)=>NULL() !< starting vertex of each part
     INTEGER,POINTER :: panparttype(:)=>NULL() !< part type (SHPP_RING if not SHPT_MULTIPATCH)
     INTEGER :: nvertices !< number of vertices
     REAL(kind=c_double),POINTER :: padfx(:)=>NULL() !< x coordinates of vertices
     REAL(kind=c_double),POINTER :: padfy(:)=>NULL() !< y coordinates of vertices
     REAL(kind=c_double),POINTER :: padfz(:)=>NULL() !< z coordinates of vertices
     REAL(kind=c_double),POINTER :: padfm(:)=>NULL() !< measure of vertices
     REAL(kind=c_double) :: dfxmin=0.0_c_double !< lower bound in x dimension
     REAL(kind=c_double) :: dfymin=0.0_c_double !< lower bound in y dimension
     REAL(kind=c_double) :: dfzmin=0.0_c_double !< lower bound in z dimension
     REAL(kind=c_double) :: dfmmin=0.0_c_double !< lower bound in measure dimension
     REAL(kind=c_double) :: dfxmax=0.0_c_double !< upper bound in x dimension
     REAL(kind=c_double) :: dfymax=0.0_c_double !< upper bound in y dimension
     REAL(kind=c_double) :: dfzmax=0.0_c_double !< upper bound in z dimension
     REAL(kind=c_double) :: dfmmax=0.0_c_double !< upper bound in measure dimension
     logical            ,POINTER :: valid(:)=>NULL() !< is node valid
     logical            ,POINTER :: actual(:)=>NULL() !< is segment actual
     integer :: nseg=0
     type(shpSeg), pointer :: currentSegment => null()
     type(shpSeg), pointer :: firstSegment => null()
     type(shpSeg), pointer :: lastSegment => null()
  END TYPE shpobject

  !TYPE(shpfileobject),PARAMETER :: shpfileobject_null = shpfileobject(0, 0)
  TYPE(shpobject),PARAMETER :: shpobject_null = shpobject(c_null_ptr, &
       0, -1, 0, &
       NULL(), NULL(), 0, NULL(), NULL(), NULL(), NULL(), &
       0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double, &
       0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double)

  !> Interface to SUBROUTINEs for reading dbf attributes.
  !! The type of the attribute can be either INTEGER,
  !! REAL(kind=c_double) (double) or CHARACTER. In case of CHARACTER
  !! attributes it is important that the length of the string passed is
  !! big enough to contain the attribute. The maximum length for each
  !! field can be obtained with the \a dbfgetfieldinfo function, but it
  !! is limited anyway to a maximum of 512 characters. The type of the
  !! attribute requested may not coincide with the native type of the
  !! field, if possible a conversion will be performed.
  !!
  !! \param hshp TYPE(shpfileobject),INTENT(inout) shapefile object to query
  !! \param ishape INTEGER,INTENT(in) the number of shape to query
  !! \param ifield INTEGER,INTENT(in) the number of field to query
  !! \param attr INTEGER, CHARACTER or REAL(kind=c_double), INTENT(out) the value of the attribute
  INTERFACE dbfreadattribute
     MODULE PROCEDURE dbfreadintegerattribute_f, dbfreaddoubleattribute_f, &
          dbfreadstringattribute_f
  END INTERFACE dbfreadattribute


  !> Interface to FUNCTIONs for setting dbf attributes.
  !! The type of the attribute can be either INTEGER,
  !! REAL(kind=c_double) (double) or CHARACTER. The type of the
  !! attribute provided may not coincide with the native type of the
  !! field, if possible a conversion will be performed. If the \a attr
  !! parameter is not provided the attribute will be set to a null
  !! value.
  !!
  !! \param hshp TYPE(shpfileobject),INTENT(inout) shapefile object to set
  !! \param ishape INTEGER,INTENT(in) the number of shape to set
  !! \param ifield INTEGER,INTENT(in) the number of field to set
  !! \param attr INTEGER, CHARACTER or REAL(kind=c_double), INTENT(in) the value of the attribute to set
  INTERFACE dbfwriteattribute
     MODULE PROCEDURE dbfwriteintegerattribute_f, dbfwritedoubleattribute_f, &
          dbfwritestringattribute_f, dbfwritenullattribute_f
  END INTERFACE dbfwriteattribute


  INTERFACE
     FUNCTION shpopen_orig(pszlayer, pszaccess) BIND(C,name='SHPOpen')
       IMPORT
       CHARACTER(kind=c_char) :: pszlayer(*)
       CHARACTER(kind=c_char) :: pszaccess(*)
       TYPE(c_ptr) :: shpopen_orig
     END FUNCTION shpopen_orig

     SUBROUTINE shpclose_orig(psshp) BIND(C,name='SHPClose')
       IMPORT
       TYPE(c_ptr),VALUE :: psshp
     END SUBROUTINE shpclose_orig

     SUBROUTINE shpgetinfo_orig(psshp, pnentities, pnshapetype, padfminbound, padfmaxbound) BIND(C,name='SHPGetInfo')
       IMPORT
       TYPE(c_ptr),VALUE :: psshp
       INTEGER(kind=c_int) :: pnentities
       INTEGER(kind=c_int) :: pnshapetype
       REAL(kind=c_double) :: padfminbound(*)
       REAL(kind=c_double) :: padfmaxbound(*)
     END SUBROUTINE shpgetinfo_orig

     FUNCTION shpcreate_orig(pszlayer, nshapetype) BIND(C,name='SHPCreate')
       IMPORT
       CHARACTER(kind=c_char) :: pszlayer(*)
       INTEGER(kind=c_int),VALUE :: nshapetype
       TYPE(c_ptr) :: shpcreate_orig
     END FUNCTION shpcreate_orig

     SUBROUTINE shpcomputeextents_int(psobject, ftnobject) BIND(C,name='SHPComputeExtentsInt')
       IMPORT
       TYPE(c_ptr),VALUE :: psobject
       TYPE(c_ptr),VALUE :: ftnobject
     END SUBROUTINE shpcomputeextents_int

     FUNCTION shpcreateobject_int(nshptype, nshapeid, nparts, panpartstart, panparttype, &
          nvertices, padfx, padfy, padfz, padfm, ftnobject) BIND(C,name='SHPCreateObjectInt')
       IMPORT
       INTEGER(kind=c_int),VALUE :: nshptype
       INTEGER(kind=c_int),VALUE :: nshapeid
       INTEGER(kind=c_int),VALUE :: nparts
       INTEGER(kind=c_int) :: panpartstart(*)
       INTEGER(kind=c_int) :: panparttype(*)
       INTEGER(kind=c_int),VALUE :: nvertices
       REAL(kind=c_double) :: padfx(*)
       REAL(kind=c_double) :: padfy(*)
       REAL(kind=c_double) :: padfz(*)
       REAL(kind=c_double) :: padfm(*)
       TYPE(c_ptr),VALUE :: ftnobject
       INTEGER(kind=c_int) :: shpcreateobject_int
     END FUNCTION shpcreateobject_int

     FUNCTION shpcreatesimpleobject_int(nshptype, nvertices, padfx, padfy, padfz, ftnobject) BIND(C,name='SHPCreateSimpleObjectInt')
       IMPORT
       INTEGER(kind=c_int),VALUE :: nshptype
       INTEGER(kind=c_int),VALUE :: nvertices
       REAL(kind=c_double) :: padfx(*)
       REAL(kind=c_double) :: padfy(*)
       REAL(kind=c_double) :: padfz(*)
       TYPE(c_ptr),VALUE :: ftnobject
       INTEGER(kind=c_int) :: shpcreatesimpleobject_int
     END FUNCTION shpcreatesimpleobject_int

     FUNCTION shpwriteobject_orig(psshp, nshapeid, psobject) BIND(C,name='SHPWriteObject')
       IMPORT
       TYPE(c_ptr),VALUE :: psshp
       INTEGER(kind=c_int),VALUE :: nshapeid
       TYPE(c_ptr),VALUE :: psobject
       INTEGER(kind=c_int) :: shpwriteobject_orig
     END FUNCTION shpwriteobject_orig

     FUNCTION shpreadobject_int(psshp, hentity, ftnobject) BIND(C,name='SHPReadObjectInt')
       IMPORT
       TYPE(c_ptr),VALUE :: psshp
       INTEGER(kind=c_int),VALUE :: hentity
       TYPE(c_ptr),VALUE :: ftnobject
       INTEGER(kind=c_int) :: shpreadobject_int
     END FUNCTION shpreadobject_int

     SUBROUTINE shpdestroyobject_orig(psshape) BIND(C,name='SHPDestroyObject')
       IMPORT
       TYPE(c_ptr),VALUE :: psshape
     END SUBROUTINE shpdestroyobject_orig

#ifndef LIBSHAPE_PRE10
     FUNCTION shprewindobject_int(hshp, psobject, ftnobject) BIND(C,name='SHPRewindObjectInt')
       IMPORT
       TYPE(c_ptr),VALUE :: hshp
       TYPE(c_ptr),VALUE :: psobject
       TYPE(c_ptr),VALUE :: ftnobject
       INTEGER(kind=c_int) :: shprewindobject_int
     END FUNCTION shprewindobject_int
#endif
  END INTERFACE

  INTERFACE
     FUNCTION dbfopen(pszfilename, pszaccess) BIND(C,name='DBFOpen')
       IMPORT
       CHARACTER(kind=c_char) :: pszfilename(*)
       CHARACTER(kind=c_char) :: pszaccess(*)
       TYPE(c_ptr) :: dbfopen
     END FUNCTION dbfopen

     SUBROUTINE dbfclose(psdbf) BIND(C,name='DBFClose')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
     END SUBROUTINE dbfclose

     FUNCTION dbfcreate(pszfilename) BIND(C,name='DBFCreate')
       IMPORT
       CHARACTER(kind=c_char) :: pszfilename(*)
       TYPE(c_ptr) :: dbfcreate
     END FUNCTION dbfcreate

     FUNCTION dbfaddfield_orig(psdbf, pszfieldname, etype, nwidth, ndecimals) BIND(C,name='DBFAddField')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       CHARACTER(kind=c_char) :: pszfieldname(*)
       INTEGER(kind=c_int),VALUE :: etype
       INTEGER(kind=c_int),VALUE :: nwidth
       INTEGER(kind=c_int),VALUE :: ndecimals
       INTEGER(kind=c_int) :: dbfaddfield_orig
     END FUNCTION dbfaddfield_orig

     FUNCTION dbfreadintegerattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadIntegerAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       INTEGER(kind=c_int) :: dbfreadintegerattribute_orig
     END FUNCTION dbfreadintegerattribute_orig

     FUNCTION dbfreaddoubleattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadDoubleAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       REAL(kind=c_double) :: dbfreaddoubleattribute_orig
     END FUNCTION dbfreaddoubleattribute_orig

     FUNCTION dbfreadstringattribute_orig(psdbf, irecord, ifield) BIND(C,name='DBFReadStringAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       TYPE(c_ptr) :: dbfreadstringattribute_orig
     END FUNCTION dbfreadstringattribute_orig

     SUBROUTINE dbfreadstringattribute_int(psdbf, irecord, ifield, attr, lattr) BIND(C,name='DBFReadStringAttributeInt')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       CHARACTER(kind=c_char) :: attr(*)
       INTEGER(kind=c_int),VALUE :: lattr
     END SUBROUTINE dbfreadstringattribute_int

     FUNCTION dbfreadlogicalattribute(psdbf, irecord, ifield) BIND(C,name='DBFReadLogicalAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       CHARACTER(kind=c_char) :: dbfreadlogicalattribute
     END FUNCTION dbfreadlogicalattribute

#ifndef LIBSHAPE_PRE10
     FUNCTION dbfisattributenull_orig(psdbf, irecord, ifield) BIND(C,name='DBFIsAttributeNULL')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       INTEGER(kind=c_int) :: dbfisattributenull_orig
     END FUNCTION dbfisattributenull_orig
#endif

     FUNCTION dbfgetfieldcount(psdbf) BIND(C,name='DBFGetFieldCount')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int) :: dbfgetfieldcount
     END FUNCTION dbfgetfieldcount

     FUNCTION dbfgetrecordcount(psdbf) BIND(C,name='DBFGetRecordCount')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int) :: dbfgetrecordcount
     END FUNCTION dbfgetrecordcount

     FUNCTION dbfgetfieldinfo_orig(psdbf, ifield, pszfieldname, pnwidth, pndecimals) BIND(C,name='DBFGetFieldInfo')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: ifield
       CHARACTER(kind=c_char) :: pszfieldname(*)
       INTEGER(kind=c_int) :: pnwidth
       INTEGER(kind=c_int) :: pndecimals
       INTEGER(kind=c_int) :: dbfgetfieldinfo_orig
     END FUNCTION dbfgetfieldinfo_orig

     FUNCTION dbfwritedoubleattribute(psdbf, irecord, ifield, dvalue) BIND(C,name='DBFWriteDoubleAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       REAL(kind=c_double),VALUE :: dvalue
       INTEGER(kind=c_int) :: dbfwritedoubleattribute
     END FUNCTION dbfwritedoubleattribute

     FUNCTION dbfwriteintegerattribute(psdbf, irecord, ifield, nvalue) BIND(C,name='DBFWriteIntegerAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       INTEGER(kind=c_int),VALUE :: nvalue
       INTEGER(kind=c_int) :: dbfwriteintegerattribute
     END FUNCTION dbfwriteintegerattribute

     FUNCTION dbfwritestringattribute(psdbf, irecord, ifield, pszvalue) BIND(C,name='DBFWriteStringAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       CHARACTER(kind=c_char) :: pszvalue(*)
       INTEGER(kind=c_int) :: dbfwritestringattribute
     END FUNCTION dbfwritestringattribute

     FUNCTION dbfwritenullattribute(psdbf, irecord, ifield) BIND(C,name='DBFWriteNULLAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       INTEGER(kind=c_int) :: dbfwritenullattribute
     END FUNCTION dbfwritenullattribute

     FUNCTION dbfwritelogicalattribute(psdbf, irecord, ifield, lvalue) BIND(C,name='DBFWriteLogicalAttribute')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: irecord
       INTEGER(kind=c_int),VALUE :: ifield
       CHARACTER(kind=c_char),VALUE :: lvalue
       INTEGER(kind=c_int) :: dbfwritelogicalattribute
     END FUNCTION dbfwritelogicalattribute

#ifndef LIBSHAPE_PRE10
     FUNCTION dbfgetnativefieldtype_orig(psdbf, ifield) BIND(C,name='DBFGetNativeFieldType')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       INTEGER(kind=c_int),VALUE :: ifield
       INTEGER(kind=c_signed_char) :: dbfgetnativefieldtype_orig
     END FUNCTION dbfgetnativefieldtype_orig

     FUNCTION dbfgetfieldindex_orig(psdbf, pszfieldname) BIND(C,name='DBFGetFieldIndex')
       IMPORT
       TYPE(c_ptr),VALUE :: psdbf
       CHARACTER(kind=c_char) :: pszfieldname(*)
       INTEGER(kind=c_int) :: dbfgetfieldindex_orig
     END FUNCTION dbfgetfieldindex_orig
#endif
  END INTERFACE

  PRIVATE
  PUBLIC shpt_null, shpt_point, shpt_arc, shpt_polygon, shpt_multipoint, &
       shpt_pointz, shpt_arcz, shpt_polygonz, shpt_multipointz, shpt_pointm, &
       shpt_arcm, shpt_polygonm, shpt_multipointm, shpt_multipatch, &
       ftstring, ftinteger, ftdouble, ftlogical, ftinvalid
  PUBLIC shpfileobject, shpobject
  PUBLIC dbfreadattribute, dbfwriteattribute
  PUBLIC shpopen, shpfileisnull, dbffileisnull, shpcreate, shpgetinfo, &
       shpreadobject, shpisnull, shpclose, shpcreatesimpleobject, shpcreateobject, &
       shpcomputeextents, shpwriteobject, shpdestroyobject, shpretraceobject,shpLoopSegment,&
       dbfgetfieldindex, dbfgetfieldinfo, dbfaddfield, dbfisattributenull, &
       dbfgetnativefieldtype
  PUBLIC shape_pnpoly, shape_ddpoly, shape_getDist, shape_getDistBox, shape_inside
  PUBLIC shape_rtodeg, shape_degtor, shape_lonlat2pos, shape_bdeb
  PUBLIC shape_dot,shape_multiply,shape_divide,shape_cross
CONTAINS


  !> It tries to open the files composing a shapefile dataset.
  !! The filename should be provided without the extension
  !! (.shp/.shx/.dbf). It tries to open all files associated, but it
  !! does not fail if part of the files are missing, so that it is
  !! possible to work on datasets not including .shp/.shx or .dbf parts.
  !! The access mode should be "rb" for reading or "rb+" for updating a
  !! file.  It returns an object of type \a shpfileobject to be used in
  !! the subsequent calls for obtaining both shp and dbf information
  !! from the dataset.  The function should not be used for creating a
  !! shapefile dataset from scratch, in that case \a shpcreate should be
  !! used.
  FUNCTION shpopen(pszshapefile, pszaccess)
    CHARACTER(len=*),INTENT(in) :: pszshapefile !< filename without extension
    CHARACTER(len=*),INTENT(in) :: pszaccess !< file access mode
    TYPE(shpfileobject) :: shpopen

    shpopen%shpfile_orig = shpopen_orig(fchartrimtostr(pszshapefile), fchartrimtostr(pszaccess))
    shpopen%dbffile_orig = dbfopen(fchartrimtostr(pszshapefile), fchartrimtostr(pszaccess))

  END FUNCTION shpopen


  !> It returns \c .TRUE. if the provided shapefile object is correctly
  !! associated with .shp/.shx file pair.  It can be called after \a
  !! shpopen to test whether .shp/.shx files have been successfully
  !! opened.
  FUNCTION shpfileisnull(hshp) RESULT(isnull)
    TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to test
    LOGICAL :: isnull

    isnull = .NOT.c_associated(hshp%shpfile_orig)

  END FUNCTION shpfileisnull


  !> It returns \c .TRUE. if the provided shapefile object is correctly
  !! associated with a .dbf file.  It can be called after \a shpopen to
  !! test whether .dbf file has been successfully opened.
  FUNCTION dbffileisnull(hshp) RESULT(isnull)
    TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to test
    LOGICAL :: isnull

    isnull = .NOT.c_associated(hshp%dbffile_orig)

  END FUNCTION dbffileisnull


  !> It creates a new, empty set of files composing a shapefile dataset.
  !! The filename should be provided without the extension
  !! (.shp/.shx/.dbf). If the files already exist, they will be
  !! overwritten.  The type of shapes should be specified using one of
  !! the constants \a shpt_*.  It returns an object of type \a
  !! shpfileobject to be used in the subsequent calls for populating the
  !! dataset both with shp and dbf information.
  FUNCTION shpcreate(pszshapefile, nshapetype)
    CHARACTER(len=*),INTENT(in) :: pszshapefile !< filename without extension
    INTEGER,INTENT(in) :: nshapetype !< type of shapes in the dataset
    TYPE(shpfileobject) :: shpcreate

    shpcreate%shpfile_orig = shpcreate_orig(fchartrimtostr(pszshapefile), nshapetype)
    shpcreate%dbffile_orig = dbfcreate(fchartrimtostr(pszshapefile))

  END FUNCTION shpcreate


  !> It gets information about the shapefile database, including dbf.
  !! If a part of the dataset has not been correctly opened
  !! (e.g. .shp/.shx or .dbf files), the corresponding information, in
  !! particular \a nentities or \a dbfrecordcount will be zero.
  SUBROUTINE shpgetinfo(hshp, nentities, shapetype, minbound, maxbound, &
       dbffieldcount, dbfrecordcount)
    TYPE(shpfileobject),INTENT(in) :: hshp !< shapefile object to query
    INTEGER,INTENT(out) :: nentities !< number of shapes
    INTEGER,INTENT(out) :: shapetype !< type of shapes in the file, one of the \a shpt_* constants
    REAL(kind=c_double),INTENT(out) :: minbound(4) !< lower bounds of shape values
    REAL(kind=c_double),INTENT(out) :: maxbound(4) !< upper bounds of shape values
    INTEGER,INTENT(out) :: dbffieldcount !< number of dbf fields
    INTEGER,INTENT(out) :: dbfrecordcount !< number of dbf records, it should be equal to \a nentities, but it is not guaranteed

    IF (.NOT.shpfileisnull(hshp)) THEN
       CALL shpgetinfo_orig(hshp%shpfile_orig, nentities, shapetype, minbound, maxbound)
    ELSE
       nentities = 0
       shapetype = 0
       minbound(:) = 0.0D0
       maxbound(:) = 0.0D0
    ENDIF
    IF (.NOT.dbffileisnull(hshp)) THEN
       dbffieldcount = dbfgetfieldcount(hshp%dbffile_orig)
       dbfrecordcount = dbfgetrecordcount(hshp%dbffile_orig)
    ELSE
       dbffieldcount = 0
       dbfrecordcount = 0
    ENDIF

  END SUBROUTINE shpgetinfo


  !> It reads a single shape from a shapefile.
  !! This function reads a single shape from a shapefile dataset (only
  !! .shp/.shx part) and it returns an object of type \a shpobject
  !! containing all the information read. The value of \a ishape should
  !! be in the range 0, \a nentites (as returned from \a shpgetinfo).
  !! The shape object returned should be destroyed with the \a
  !! shpdestroyobject subroutine before being reused, in order to avoid
  !! memory leaks.
  FUNCTION shpreadobject(hshp, ishape)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to read from
    INTEGER :: ishape !< number of shape to be read
    TYPE(shpobject),TARGET :: shpreadobject

    TYPE(shpobject) :: lshpobject

    INTEGER :: ier

    IF (.NOT.shpfileisnull(hshp)) THEN
       ier = shpreadobject_int(hshp%shpfile_orig, ishape, C_LOC(shpreadobject))
    ELSE ! initialize to empty
       shpreadobject = shpobject_null
    ENDIF

  END FUNCTION shpreadobject


  !> It returns \c .TRUE. if the provided shape object is not valid.
  !! It can be called after \a shpreadobject to test whether
  !! a valid shape has ben read.
  FUNCTION shpisnull(psobject) RESULT(isnull)
    TYPE(shpobject),INTENT(in) :: psobject !< shape object to test
    LOGICAL :: isnull

    isnull = .NOT.c_associated(psobject%shpobject_orig)

  END FUNCTION shpisnull


  !> It closes all the files associated with the shapefile dataset.
  SUBROUTINE shpclose(hshp)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to be closed
    
    IF (.NOT.shpfileisnull(hshp)) THEN
       CALL shpclose_orig(hshp%shpfile_orig)
       hshp%shpfile_orig = c_null_ptr
    ENDIF
    IF (.NOT.dbffileisnull(hshp)) THEN
       CALL dbfclose(hshp%dbffile_orig)
       hshp%dbffile_orig = c_null_ptr
    ENDIF
    
  END SUBROUTINE shpclose

  subroutine shpClearSegment(shpObj)
    TYPE(shpobject),INTENT(inout) :: shpObj !< shapefile object to be closed
    type(shpSeg), pointer :: cs, ocs
    if (.not.associated(shpObj%firstSegment).or..not.associated(shpObj%lastSegment))then
       if (associated(shpObj%firstSegment))deallocate(shpObj%firstSegment)
       if (associated(shpObj%lastSegment))deallocate(shpObj%lastSegment)
       allocate(shpObj%firstSegment,shpObj%lastSegment)
    else
       cs => shpObj%firstSegment%next
       do while (.not.associated(cs,target=shpObj%lastSegment))
          ocs => cs%next
          deallocate(cs)
          cs => ocs
       end do
    end if
    shpObj%nseg=0
    shpObj%firstSegment%next => shpObj%lastSegment
    shpObj%lastSegment%prev => shpObj%firstSegment
    return
  end subroutine shpClearSegment
  
  !> It creates a new shape object, simple version.
  !! It creates a new shape object and returns it as a variable of type
  !! \a shpobject; the object has x,y,z coordinates with no measure and
  !! a single part. The successful creation can be checked with the
  !! function \a shpisnull.
  FUNCTION shpcreatesimpleobject(nshptype, nvertices, padfx, padfy, padfz)
    INTEGER :: nshptype !< type of shape, one of the \a shpt_* constants
    INTEGER :: nvertices !< number of vertices
    REAL(kind=c_double) :: padfx(nvertices) !< x coordinates
    REAL(kind=c_double) :: padfy(nvertices) !< y coordinates
    REAL(kind=c_double),OPTIONAL :: padfz(nvertices) !< z coordinates, it can be skipped
    TYPE(shpobject),TARGET :: shpcreatesimpleobject

    TYPE(shpobject) :: lshpobject

    IF (shpcreatesimpleobject_int(nshptype, nvertices, padfx, padfy, padfz, &
         C_LOC(shpcreatesimpleobject)) /= 0) THEN
       shpcreatesimpleobject = shpobject_null
    ENDIF

  END FUNCTION shpcreatesimpleobject


  !> It creates a new shape object, full version.
  !! It creates a new shape object and returns it as a variable of type
  !! \a shpobject; the object has x,y,z coordinates with measure and
  !! possibly multiple parts. The successful creation can be checked
  !! with the function \a shpisnull.
  FUNCTION shpcreateobject(nshptype, ishape, nparts, panpartstart, panparttype, &
       nvertices, padfx, padfy, padfz, padfm)
    INTEGER :: nshptype !< type of shape, one of the \a shpt_* constants
    INTEGER :: ishape !< shapeid to be recorded with this shape
    INTEGER :: nparts !< number of parts
    INTEGER :: nvertices !< number of vertices
    INTEGER :: panpartstart(nparts) !< start indices of each part
    INTEGER :: panparttype(nparts) !< type of each of the parts, this is only meaningful for \a MULTIPATCH files, for all other cases it will be assumed to be \a SHPP_RING 
    REAL(kind=c_double) :: padfx(nvertices) !< x coordinates
    REAL(kind=c_double) :: padfy(nvertices) !< y coordinates
    REAL(kind=c_double),OPTIONAL :: padfz(nvertices) !< z coordinates, it can be skipped
    REAL(kind=c_double),OPTIONAL :: padfm(nvertices) !< measure, it can be skipped
    TYPE(shpobject),TARGET :: shpcreateobject

    TYPE(shpobject) :: lshpobject

    IF (shpcreateobject_int(nshptype, ishape, nparts, panpartstart, panparttype, &
         nvertices, padfx, padfy, padfz, padfm, C_LOC(shpcreateobject)) /= 0) THEN
       shpcreateobject = shpobject_null
    ENDIF

  END FUNCTION shpcreateobject


  !> It recomputes the extents of a shape.
  !! This subroutine replaces the existing values of the dfXMin, dfYMin,
  !! dfZMin, dfMMin, dfXMax, dfYMax, dfZMax, and dfMMax with updated
  !! values based on the current set of vertices for the shape. It is
  !! automatically called by shpcreateobject, but if the vertices of an
  !! existing object are altered it should be called again to fix up the
  !! extents.
  SUBROUTINE shpcomputeextents(psobject)
    TYPE(shpobject),TARGET :: psobject !< shape object to update

    CALL shpcomputeextents_int(psobject%shpobject_orig, C_LOC(psobject))

  END SUBROUTINE shpcomputeextents


  !> It writes a shape object to a file.
  !! It returns the number of shape written, starting from 0, or -1 in
  !! case of failure.
  FUNCTION shpwriteobject(hshp, ishape, psobject)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object where to write
    INTEGER :: ishape !< number of shape to write in the file, starting from 0, -1 to append to existing shapes
    TYPE(shpobject) :: psobject !< shape object to be written
    INTEGER :: shpwriteobject

    IF (.NOT.shpfileisnull(hshp)) THEN
       shpwriteobject = shpwriteobject_orig(hshp%shpfile_orig, ishape, psobject%shpobject_orig)
    ELSE
       shpwriteobject = 0
    ENDIF

  END FUNCTION shpwriteobject


  !> It destroys a shape object for subsequent reuse or when not used anymore.
  SUBROUTINE shpdestroyobject(psobject)
    TYPE(shpobject) :: psobject !< shape object to be destroyed

    call shpClearSegment(psObject)
    if (associated(psObject%firstSegment)) deallocate(psObject%firstSegment)
    if (associated(psObject%lastSegment)) deallocate(psObject%lastSegment)
    nullify(psObject%currentSegment)
    if (associated(psObject%valid)) deallocate(psObject%valid)
    if (associated(psObject%actual)) deallocate(psObject%actual)
    IF (c_associated(psobject%shpobject_orig)) THEN
       CALL shpdestroyobject_orig(psobject%shpobject_orig)
    ENDIF
    psobject = shpobject_null

  END SUBROUTINE shpdestroyobject


#ifndef LIBSHAPE_PRE10
  !> It sets the correct ring order.
  !! This function will reverse any ring necessary in order to enforce
  !! the shapefile restrictions on the required order of inner and outer
  !! rings in the Shapefile specification. It returns TRUE if a change
  !! is made and FALSE if no change is made. Only polygon objects will
  !! be affected though any object may be passed.
  !! This procedure is available only with libshape version 1.2.10 or
  !! later.
  FUNCTION shprewindobject(hshp, psobject)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object (not used)
    TYPE(shpobject),INTENT(inout),TARGET :: psobject !< shape object to be rewound
    LOGICAL :: shprewindobject

    INTEGER :: ier

    if (shape_bdeb) write(*,*) "Rewinding rings in shape."
    ier = shprewindobject_int(hshp%shpfile_orig, psobject%shpobject_orig, &
         C_LOC(psobject))
    IF (ier == 0) THEN
       shprewindobject = .FALSE.
    ELSE
       shprewindobject = .TRUE.
    ENDIF

  END FUNCTION shprewindobject

  logical function shpLoopSegment(shpObj,spos,epos)
    TYPE(shpobject) :: shpObj !< shape object to be destroyed
    real :: spos(3),epos(3)
    if (.not.associated(shpObj%currentSegment)) then
       shpObj%currentSegment =>  shpObj%firstSegment%next 
    else
       shpObj%currentSegment =>  shpObj%currentSegment%next
    end if
    if (associated(shpObj%currentSegment,target=shpObj%lastSegment)) then
       nullify(shpObj%currentSegment)
       shploopSegment=.false.
    else
       spos(1)=shpObj%currentSegment%spos(1)
       spos(2)=shpObj%currentSegment%spos(2)
       spos(3)=shpObj%currentSegment%spos(3)
       epos(1)=shpObj%currentSegment%epos(1)
       epos(2)=shpObj%currentSegment%epos(2)
       epos(3)=shpObj%currentSegment%epos(3)
       shploopSegment=.true.
    end if
    return
  end function shpLoopSegment
  
  !> It returns the index of the field matching the name.
  !! The comparison is case insensitive, however lengths must match
  !! exactly. It returns -1 if the field is not found or if the shape
  !! object is not valid.
  !! This procedure is available only with libshape version 1.2.10 or
  !! later.
  FUNCTION dbfgetfieldindex(hshp, pszfieldname)
    TYPE(shpfileobject),INTENT(in) :: hshp !< shape object to query
    CHARACTER(len=*),INTENT(in) :: pszfieldname !< field name to search
    INTEGER :: dbfgetfieldindex

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfgetfieldindex = dbfgetfieldindex_orig(hshp%dbffile_orig, fchartrimtostr(pszfieldname))
    ELSE
       dbfgetfieldindex = -1
    ENDIF

  END FUNCTION dbfgetfieldindex
#endif


  !> It returns information about a dbf field.
  !! The return value is the type of the requested field, which is one
  !! of the \a ft* constants. The field type returned does not
  !! correspond one to one with the xBase field types. For instance the
  !! xBase field type for Date will just be returned as being
  !! ftinteger. It returns -1 if the shape object is not valid.
  FUNCTION dbfgetfieldinfo(hshp, ifield, pszfieldname, pnwidth, pndecimals)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
    INTEGER,INTENT(in) :: ifield !< number of field to query, in the interval 0, nfield - 1
    CHARACTER(len=*),INTENT(out) :: pszfieldname !< the name of the field, it can be up to 11 characters long
    INTEGER,INTENT(out) :: pnwidth !< the width of the field in characters
    INTEGER,INTENT(out) :: pndecimals !< the number of decimals in a floating point representation, nonzero only for fields of type \a ftdouble
    INTEGER :: dbfgetfieldinfo

    CHARACTER(len=11) :: lpszfieldname

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfgetfieldinfo = dbfgetfieldinfo_orig(hshp%dbffile_orig, ifield, &
            lpszfieldname, pnwidth, pndecimals)
       pszfieldname = lpszfieldname ! must strip null here!
    ELSE
       dbfgetfieldinfo = -1
    ENDIF

  END FUNCTION dbfgetfieldinfo


  !> It adds a new field to an existing dataset.
  !! Note that fields can only be added to datasets with no dbf records,
  !! though this is limitation of this API, not of the file format. This
  !! function returns the number of the new field, starting from 0, or
  !! -1 in case of error.
  FUNCTION dbfaddfield(hshp, pszfieldname, etype, nwidth, ndecimals)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to update
    CHARACTER(len=*),INTENT(in) :: pszfieldname !< the name of the new field, at most 11 characters will be used
    INTEGER,INTENT(in) :: etype !< the type of the new field, one of the \a ft* constants
    INTEGER,INTENT(in) :: nwidth !< the width of the field to be created in characters
    INTEGER,INTENT(in) :: ndecimals !< the number of decimals in a floating point representation for fields of type \a ftdouble, for the other types it should be 0
    INTEGER :: dbfaddfield

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfaddfield = dbfaddfield_orig(hshp%dbffile_orig, fchartrimtostr(pszfieldname), &
            etype, nwidth, ndecimals)
    ELSE
       dbfaddfield = -1
    ENDIF

  END FUNCTION dbfaddfield


  SUBROUTINE dbfreadintegerattribute_f(hshp, ishape, ifield, attr)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    INTEGER,INTENT(out) :: attr

    IF (.NOT.dbffileisnull(hshp)) THEN
       attr = dbfreadintegerattribute_orig(hshp%dbffile_orig, ishape, ifield)
    ELSE
       attr = 0
    ENDIF

  END SUBROUTINE dbfreadintegerattribute_f


  SUBROUTINE dbfreaddoubleattribute_f(hshp, ishape, ifield, attr)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    REAL(kind=c_double),INTENT(out) :: attr

    IF (.NOT.dbffileisnull(hshp)) THEN
       attr = dbfreaddoubleattribute_orig(hshp%dbffile_orig, ishape, ifield)
    ELSE
       attr = 0.0_c_double
    ENDIF

  END SUBROUTINE dbfreaddoubleattribute_f


  SUBROUTINE dbfreadstringattribute_f(hshp, ishape, ifield, attr)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    CHARACTER(len=*),INTENT(out) :: attr

    IF (.NOT.dbffileisnull(hshp)) THEN
       attr = strtofchar(dbfreadstringattribute_orig(hshp%dbffile_orig, ishape, ifield), LEN(attr))
    ELSE
       attr = ''
    ENDIF

  END SUBROUTINE dbfreadstringattribute_f


#ifndef LIBSHAPE_PRE10
  !> It returns \c .TRUE. if the requested attribute is NULL valued.
  !! Note that NULL fields are represented in the .dbf file as having
  !! all spaces in the field. Reading NULL fields will result in a value
  !! of 0.0 or an empty string with the dbfreadattribute interface.
  !! It returns \c .TRUE. in case of error as well.
  !! This procedure is available only with libshape version 1.2.10 or
  !! later.
  FUNCTION dbfisattributenull(hshp, ishape, ifield)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
    INTEGER,INTENT(in) :: ishape !< number of shape (record) to query
    INTEGER,INTENT(in) :: ifield !< number of field to query
    LOGICAL :: dbfisattributenull

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfisattributenull = dbfisattributenull_orig(hshp%dbffile_orig, ishape, ifield) == 0
    ELSE ! force to null
       dbfisattributenull = .FALSE.
    ENDIF

  END FUNCTION dbfisattributenull
#endif


  FUNCTION dbfwriteintegerattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    INTEGER,INTENT(in) :: attr
    INTEGER :: dbfwriteattribute

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfwriteattribute = dbfwriteintegerattribute(hshp%dbffile_orig, ishape, ifield, attr)
    ELSE
       dbfwriteattribute = 0
    ENDIF

  END FUNCTION dbfwriteintegerattribute_f


  FUNCTION dbfwritedoubleattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    REAL(kind=c_double),INTENT(in) :: attr
    INTEGER :: dbfwriteattribute

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfwriteattribute = dbfwritedoubleattribute(hshp%dbffile_orig, ishape, ifield, attr)
    ELSE
       dbfwriteattribute = 0
    ENDIF

  END FUNCTION dbfwritedoubleattribute_f


  FUNCTION dbfwritestringattribute_f(hshp, ishape, ifield, attr) RESULT(dbfwriteattribute)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    CHARACTER(len=*),INTENT(in) :: attr
    INTEGER :: dbfwriteattribute

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfwriteattribute = dbfwritestringattribute(hshp%dbffile_orig, ishape, ifield, fchartostr(attr))
    ELSE
       dbfwriteattribute = 0
    ENDIF

  END FUNCTION dbfwritestringattribute_f


  FUNCTION dbfwritenullattribute_f(hshp, ishape, ifield) RESULT(dbfwriteattribute)
    TYPE(shpfileobject),INTENT(inout) :: hshp
    INTEGER,INTENT(in) :: ishape, ifield
    INTEGER :: dbfwriteattribute

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfwriteattribute = dbfwritenullattribute(hshp%dbffile_orig, ishape, ifield)
    ELSE
       dbfwriteattribute = 0
    ENDIF

  END FUNCTION dbfwritenullattribute_f


#ifndef LIBSHAPE_PRE10
  !> It returns the dbf type code of the requested field.
  !! The return value is a single character and it can assume the
  !! following values:
  !!
  !! - 'C' (String)
  !! - 'D' (Date)
  !! - 'F' (Float)
  !! - 'N' (Numeric, with or without decimal)
  !! - 'L' (Logical)
  !! - 'M' (Memo: 10 digits .DBT block ptr)
  !! - ' ' (field out of range or other error)
  !!
  !! This procedure is available only with libshape version 1.2.10 or
  !! later.
  FUNCTION dbfgetnativefieldtype(hshp, ifield)
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to query
    INTEGER,INTENT(in) :: ifield !< number of field to query
    CHARACTER(len=1) :: dbfgetnativefieldtype

    IF (.NOT.dbffileisnull(hshp)) THEN
       dbfgetnativefieldtype = CHAR(dbfgetnativefieldtype_orig(hshp%dbffile_orig, ifield))
    ELSE ! force to null
       dbfgetnativefieldtype = ' '
    ENDIF

  END FUNCTION dbfgetnativefieldtype
#endif


  SUBROUTINE shpsetobjectfortran(ftnobject, cobject, nshptype, nshapeid, &
       nparts, panpartstart, panparttype, &
       nvertices, padfx, padfy, padfz, padfm, &
       dfxmin, dfymin, dfzmin, dfmmin, dfxmax, dfymax, dfzmax, dfmmax) &
       BIND(C,name='SHPSetObjectFortran')
    TYPE(c_ptr),VALUE :: ftnobject
    TYPE(c_ptr),VALUE :: cobject
    INTEGER(kind=c_int) :: nshptype ! Shape Type (SHPT_* - see list above)
    INTEGER(kind=c_int) :: nshapeid ! Shape Number (-1 is unknown/unassigned)
    INTEGER(kind=c_int) :: nparts ! # of Parts (0 implies single part with no info)
    INTEGER(kind=c_int),TARGET :: panpartstart(nparts), & ! Start Vertex of part
         panparttype(nparts) ! Part Type (SHPP_RING if not SHPT_MULTIPATCH)
    INTEGER(kind=c_int) :: nvertices ! Vertex list 
    REAL(kind=c_double),TARGET ::  padfx(nvertices), padfy(nvertices), &
         padfz(nvertices), padfm(nvertices) ! (all zero if not provided)
    REAL(kind=c_double) :: & ! Bounds in X, Y, Z and M dimensions
         dfxmin, dfymin, dfzmin, dfmmin, dfxmax, dfymax, dfzmax, dfmmax

    TYPE(shpobject),POINTER :: obj

    CALL C_F_POINTER(ftnobject, obj)

    obj%shpobject_orig = cobject
    obj%nshptype = nshptype
    obj%nshapeid = nshapeid
    obj%nparts = nparts
    obj%panpartstart => panpartstart
    obj%panparttype => panparttype
    obj%nvertices = nvertices
    obj%padfx => padfx
    obj%padfy => padfy
    obj%padfz => padfz
    obj%padfm => padfm
    obj%dfxmin = dfxmin
    obj%dfymin = dfymin
    obj%dfzmin = dfzmin
    obj%dfmmin = dfmmin
    obj%dfxmax = dfxmax
    obj%dfymax = dfymax
    obj%dfzmax = dfzmax
    obj%dfmmax = dfmmax

  END SUBROUTINE shpsetobjectfortran

  subroutine shpReTraceObject(hshp, shpobj,leps,eps)
    implicit none
    TYPE(shpfileobject),INTENT(inout) :: hshp !< shapefile object to read from
    TYPE(shpobject),INTENT(inout) :: shpobj !< shape object to test
    logical :: leps
    real :: eps
    !
    logical :: cl    ! last node in ring
    integer :: cii   ! start index
    real :: cpos(3)  ! start node of ring
    logical :: ol    ! does previous ring exist
    integer :: oii   ! stop index
    real :: opos(3)  ! stop node of previous ring
    type(shpSeg), pointer :: cs
    integer :: ii
    call shpClearSegment(shpobj)
    ol=.false.   ! previous ring does not exist
    oii=0
    cl=.true.    ! should we start a new ring?
    cii=0
    if (shape_bdeb) write(*,*)'Initial number of vertices:',&
         & shpobj%nvertices,leps
    if (associated(shpobj%valid)) deallocate(shpobj%valid)
    if (associated(shpobj%actual)) deallocate(shpobj%actual)
    allocate(shpobj%valid(shpobj%nvertices),&
         & shpobj%actual(shpobj%nvertices))
    do ii=1,shpobj%nvertices
       shpobj%valid(ii) = .TRUE.
       shpobj%actual(ii)= .TRUE.
    end do
    do ii=1,shpobj%nvertices
       if (cl) then
          cl=.false. ! do not start a new ring until we find end of this ring
          if (ol) then ! add segment c->o to front of list
             allocate(cs)
             cs%spos(1)=shpobj%padfx(ii) ! start point
             cs%spos(2)=shpobj%padfy(ii)
             cs%spos(3)=shpobj%padfz(ii)
             cs%epos(1)=opos(1) ! end point
             cs%epos(2)=opos(2)
             cs%epos(3)=opos(3)
             ! add to front of list
             shpObj%nseg=shpObj%nseg+1
             cs%next=>shpObj%firstSegment%next
             cs%prev=>shpObj%firstSegment
             shpObj%firstSegment%next%prev => cs
             shpObj%firstSegment%next => cs
             nullify(cs)
             ! simplify ring...
             if (leps) then
                call shape_simplify(shpObj,cii,oii,eps)
             end if
          end if
          cii=ii
          cpos(1)=shpobj%padfx(ii)
          cpos(2)=shpobj%padfy(ii)
          cpos(3)=shpobj%padfz(ii)
       else if (cpos(1).eq.shpobj%padfx(ii).and.cpos(2).eq.shpobj%padfy(ii)) then
          shpObj%actual(ii)=.false. ! next segment is not actual
          cl=.true. ! start a new ring
          ol=.true. ! previous ring exists
          oii=ii
          opos(1)=shpobj%padfx(ii)
          opos(2)=shpobj%padfy(ii)
          opos(3)=shpobj%padfz(ii)
          if (leps) then
             call shape_simplify(shpObj,cii,oii,eps)
          end if
       end if
    end do 
    if (.not.cl ) then ! last ring was not closed properly
       if (leps) then
          oii=ii
          call shape_simplify(shpObj,cii,oii,eps)
       end if
       ! close ring
       allocate(cs)
       cs%spos(1)=shpobj%padfx(ii)! start point
       cs%spos(2)=shpobj%padfy(ii)
       cs%spos(3)=shpobj%padfz(ii)
       cs%epos(1)=cpos(1) ! end point
       cs%epos(2)=cpos(2)
       cs%epos(3)=cpos(3)
       ! add to front of list
       shpObj%nseg=shpObj%nseg+1
       cs%next=>shpObj%firstSegment%next
       cs%prev=>shpObj%firstSegment
       shpObj%firstSegment%next%prev => cs
       shpObj%firstSegment%next => cs
       nullify(cs)
    end if
    ! check if rings are already closed (if so delete all segments)
    if (shpObj%nseg .gt. 0) then
       if (shpObj%firstSegment%next%spos(1).eq.shpObj%lastSegment%prev%epos(1).and. &
            shpObj%firstSegment%next%spos(2).eq.shpObj%lastSegment%prev%epos(2).and. &
            shpObj%firstSegment%next%spos(3).eq.shpObj%lastSegment%prev%epos(3)) then
          call shpClearSegment(shpObj)
       end if
    end if
    return
  end subroutine shpReTraceObject
  
  ! SUBROUTINE PNPOLY C
  ! PURPOSE
  ! TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
  !
  ! USAGE
  ! CALL PNPOLY (PX, PY, N, XX, YY, INOUT )
  !
  ! DESCRIPTION OF THE PARAMETERS
  !
  ! PX - X-COORDINATE OF POINT IN QUESTION.
  !
  ! PY - Y-COORDINATE OF POINT IN QUESTION.
  !
  ! N - NUMBER OF VERTICES IN THE POLYGON.
  !
  ! XX - N LONG VECTOR CONTAINING X-COORDINATES OF VERTICES OF POLYGON.
  !
  ! YY - N LONG VECTOR CONTAING Y-COORDINATES OF VERTICES OF POLYGON.
  !
  ! INOUT - THE SIGNAL RETURNED:
  !
  ! -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
  !
  ! 0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
  !
  ! 1 IF THE POINT IS INSIDE OF THE POLYGON.
  !
  ! REMARKS
  ! THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
  ! THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
  ! OPTIONALLY BE INCREASED BY 1.
  ! THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
  ! OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
  ! OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
  ! N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
  ! INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
  ! THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
  ! WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
  !
  !
  !     ..................................................................
  !
  !        SUBROUTINE PNPOLY
  !
  !        PURPOSE
  !           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
  !
  !        USAGE
  !           CALL PNPOLY (PX, PY, N, XX, YY, INOUT )
  !
  !        DESCRIPTION OF THE PARAMETERS
  !           PX      - X-COORDINATE OF POINT IN QUESTION.
  !           PY      - Y-COORDINATE OF POINT IN QUESTION.
  !           N       - NUMBER OF VERTICES IN THE POLYGON.
  !           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF
  !                     VERTICES OF POLYGON.
  !           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF
  !                     VERTICES OF POLYGON.
  !           INOUT   - THE SIGNAL RETURNED:
  !                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
  !                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
  !                      1 IF THE POINT IS INSIDE OF THE POLYGON.
  !
  !        REMARKS
  !           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
  !           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
  !           OPTIONALLY BE INCREASED BY 1.
  !           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
  !           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
  !           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
  !           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
  !           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
  !           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
  !           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
  !
  !        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
  !           NONE
  !
  !        METHOD
  !           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
  !           SHAPE_CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
  !           POINT IS INSIDE OF THE POLYGON.
  !
  !     ..................................................................
  !
  SUBROUTINE shape_pnpoly(PX,PY,N,X,Y,INOUT)
    ! Run semi-infinite ray horizontally (increasing x, fixed y)
    ! out from the test point, and count how many edges it crosses.
    ! At each crossing, the ray switches between inside and outside.
    ! This is called the Jordan curve theorem. 
    implicit none
    integer n,inout
    REAL PX,PY,X(N),Y(N),MM
    LOGICAL MX,MY,NX,NY
    INTEGER :: i,j,unt,cnt=0
    INOUT=-1 ! point is outside
    if (n.le.1) return
    i=1
    j=2
    LOOP: DO I=1,N
       J=1+MOD(I,N)
       mx=x(i).ge.px
       nx=x(j).ge.px
       my=y(i).ge.py
       ny=y(j).ge.py
       ! find out if segment crosses horisontal ray to the right og the point
       if ((my.and..not.ny).or.(.not.my.and.ny)) then ! crosses left or right
          if ((mx.and..not.nx).or.(.not.mx.and.nx)) then ! may cross right
             MM=(px-x(i))-(x(j)-x(i))*(py-y(i))/(y(j)-y(i))
             if (abs(mm).lt.1.0D-10) then ! point on segment (undetermined)
                inout=0
                if (shape_bdeb)write(*,*) 'Segment crossing:',i,x(i)-px,y(i)-py,j,x(j)-px,y(j)-py,inout
                return
             else if (MM .lt. 0.0D0) then ! crosses right of point
                inout=-inout
                if (shape_bdeb)write(*,*) 'Proximity crossing:',i,x(i)-px,y(i)-py,j,x(j)-px,y(j)-py,inout
             else
                if (shape_bdeb)write(*,*) 'Close crossing:',i,x(i)-px,y(i)-py,j,x(j)-px,y(j)-py,inout
             end if
          else if (mx.and.nx) then ! crosses right of point for sure
             inout=-inout
             if (shape_bdeb)write(*,*) 'Clear crossing:',i,x(i)-px,y(i)-py,j,x(j)-px,y(j)-py,inout
          end if
       end if
    end do LOOP
    if (shape_bdeb) then
       cnt=cnt+1
       unt=10+mod(cnt,50)
       if (inout.eq.-1) then
          write(*,*) 'Outside:',px,py,inout,unt
       else if (inout.eq.0) then
          write(*,*) 'Border:',px,py,inout,unt
       else
          write(*,*) 'Inside:',px,py,inout,unt
       end if
#ifdef JNK
       DO I=1,N
          J=1+MOD(I,N)
          mx=x(i).ge.px
          nx=x(j).ge.px
          my=y(i).ge.py
          ny=y(j).ge.py
          write(unt,'(2(2X,I0,X,2(F12.5),2L1))') i,x(i)-px,y(i)-py,mx,my,&
               & j,x(j)-px,y(j)-py,nx,ny
       end do
#endif
    end if
  end SUBROUTINE shape_pnpoly
  SUBROUTINE shape_pnpoly_old(PX,PY,N,XX,YY,INOUT)
    implicit none
    integer n,inout
    REAL PX,PY,XX(N),YY(N)
    LOGICAL MX,MY,NX,NY
    INTEGER i,j, old
    real X(n),Y(n),mm
    old=-10
    INOUT=-1 ! point is outside
    if (n.le.1) return
    DO I=1,N
       X(I)=XX(I)-PX
       Y(I)=YY(I)-PY
    end do
    i=1
    j=2
    if (shape_bdeb)write(*,*) 'Loop:',i,x(i),y(i),j,x(j),y(j),inout
    LOOP: DO I=1,N
       J=1+MOD(I,N)
       MX=X(I).GE.0.0
       NX=X(J).GE.0.0
       MY=Y(I).GE.0.0
       NY=Y(J).GE.0.0
       IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) CYCLE LOOP
       IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) then
          mm=(Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))
          if (shape_bdeb)write(*,*) 'MM:',mm,inout
          IF(mm.lt.0) then
             cycle loop
          else if (mm.eq.0) then
             inout=0 ! point is on a vertex
             return
          else if (mm.gt.0) then
             INOUT=-INOUT
             if (shape_bdeb)write(*,*) 'Loop:',i,x(i),y(i),j,x(j),y(j),inout
             cycle loop
          end if
       end if
       INOUT=-INOUT
       if (shape_bdeb)write(*,*) 'Loop:',i,x(i),y(i),j,x(j),y(j),inout
       CYCLE LOOP
    end do LOOP
    RETURN
  END SUBROUTINE shape_pnpoly_old
  !
  real function shape_ddpoly(POS,N,XPOS,ACTUAL)
    ! gives smalles distance to cartesian polygon
    implicit none
    integer n
    real pos(3),xpos(3,n)
    logical actual(n)
    INTEGER i,j
    real dd,ddmin
    logical llmin
    shape_ddpoly=-1.0D0
    llmin=.false.
    ddmin=-1.0D0
    if (n.le.1) return
    LOOP: DO I=1,N
       J=1+MOD(I,N)
       if (actual(i)) then
          dd=shape_getDist(pos,xpos(1,i),xpos(1,j))
          if (llmin) then
             ddmin=min(ddmin,dd)
          else
             ddmin=dd
             llmin=.true.
          end if
       end if
    end do LOOP
    shape_ddpoly=ddmin
    RETURN
  END function shape_ddpoly
  !
  REAL(kind=8) FUNCTION length2(x)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(2) :: x 
    length2 = dsqrt(DOT_PRODUCT(x,x))
  END FUNCTION length2

  ! Calcule la distance d'un point P a un segment (V1,V2)
  ! Input     vP  : coordonnees du point P
  !           v1  : coordonnees du debut du segment V1
  !           v2  : coordonnees de la fin du segment V2
  function perpendicularDistance(vP, v1, v2)
    implicit none
    REAL(kind=8)              :: perpendicularDistance
    REAL(kind=8),DIMENSION(2) :: vP,v1,v2,LI,LP
    LI = v2 - v1
    LP = vP - v1
    perpendicularDistance = abs( LI(1) * LP(2) - LP(1) * LI(2)) / length2(LI)
  end function perpendicularDistance

  ! simplification of a polygon based on Ramer–Douglas–Peucker algorithm
  ! Input    v(2,n)   : coordinates of the vertices
  !          i1       : indice of first vertex
  !          i2       : indice of last vertex
  !          eps      : distance of simplification
  ! Output   valid(n) : boolean to know if this vertex belongs or not to the simplified polygon
  recursive subroutine shape_DouglasPeuckerRecursive(v, i1, i2, eps, valid)
    implicit none
    INTEGER,INTENT(IN)                   :: i1,i2
    REAL(kind=8),INTENT(IN)              :: eps
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v
    LOGICAL,DIMENSION(:),POINTER         :: valid
    INTEGER                              :: ip,imax
    REAL(kind=8)                         :: dist,dmax
    dmax = 0.D0
    imax = 1
    ! regarde les points intermediaires
    ! et calcule la distance perpenticulaire maximale
    DO ip = i1+1,i2-1
       dist = perpendicularDistance(v(:,ip), v(:,i1), v(:,i2))
       IF ( dist > dmax ) THEN
          imax = ip
          dmax = dist
       ENDIF
    ENDDO
    ! si la distance perpenticulaire maximale est plus grande que eps, 
    ! simplifie les 2 branches [i1,imax] et [imax,i2]
    IF ( dmax > eps ) THEN
       call shape_DouglasPeuckerRecursive(v, i1, imax, eps, valid)
       call shape_DouglasPeuckerRecursive(v, imax, i2, eps, valid)
       ! sinon on vire tous les points entre i1 et i2   
    ELSE
       valid(i1+1:i2-1) = .FALSE.
    ENDIF
  end subroutine shape_DouglasPeuckerRecursive

  real function log2(x)
    implicit none
    real, intent(in) :: x
    log2 = log(x) / log(2.)
  end function log2

  ! simplification of a polygon based on Ramer–Douglas–Peucker algorithm
  ! Input    v(2,n)   : coordinates of the vertices
  !          i1       : indice of first vertex
  !          i2       : indice of last vertex
  !          eps      : distance of simplification
  ! Output   valid(n) : boolean to know if this vertex belongs or not to the simplified polygon
  recursive subroutine shape_DouglasPeuckerIteratif(v, eps, valid)
    implicit none
    REAL(kind=8),INTENT(IN)              :: eps
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v
    LOGICAL,DIMENSION(:),POINTER         :: valid
    INTEGER                              :: i1,i2
    INTEGER                              :: ip,imax
    REAL(kind=8)                         :: dist,dmax
    integer, dimension(:,:), pointer     :: loop_index
    integer                              :: offset_loop_index
    integer                              :: next_loop_index
    integer                              :: nbp
    integer                              :: nb_sub_step
    nbp = size(valid)
    nb_sub_step = 2 ** ( ceiling( log2( real(nbp) - 1 ) ) + 1 )
    allocate( loop_index( 2, nb_sub_step ) )
    loop_index = -1
    loop_index( : , 1 ) = (/ 1, nbp /)
    ! print *, loop_index
    offset_loop_index = 1
    next_loop_index   = 1
    i1 = loop_index( 1, offset_loop_index )
    i2 = loop_index( 2, offset_loop_index )
    do while( (i1 /= -1) .and. (i2 /= -1) .and. (offset_loop_index <= nb_sub_step) )
       dmax = 0.D0
       imax = 1
       ! regarde les points intermediaires
       ! et calcule la distance perpenticulaire maximale
       DO ip = i1+1,i2-1
          dist = perpendicularDistance(v(:,ip), v(:,i1), v(:,i2))
          IF ( dist > dmax ) THEN
             imax = ip
             dmax = dist
          ENDIF
       ENDDO
       ! si la distance perpenticulaire maximale est plus grande que eps, 
       ! simplifie les 2 branches [i1,imax] et [imax,i2]
       IF ( dmax > eps ) THEN
          ! call shape_DouglasPeuckerRecursive(v, i1, imax, eps, valid)
          next_loop_index = next_loop_index + 1
          loop_index( 1, next_loop_index ) = i1
          loop_index( 2, next_loop_index ) = imax
          ! call shape_DouglasPeuckerRecursive(v, imax, i2, eps, valid)
          next_loop_index = next_loop_index + 1
          loop_index( 1, next_loop_index ) = imax
          loop_index( 2, next_loop_index ) = i2
          ! sinon on vire tous les points entre i1 et i2   
       ELSE
          valid(i1+1:i2-1) = .FALSE.
       ENDIF
       offset_loop_index = offset_loop_index + 1
       i1 = loop_index( 1, offset_loop_index )
       i2 = loop_index( 2, offset_loop_index )
    end do  ! END - do while
    deallocate(loop_index)
  end subroutine shape_DouglasPeuckerIteratif

  ! Does not work over 180-meridian
  subroutine shape_simplify(shpobj,cii,oii,eps)
    IMPLICIT NONE
    TYPE(shpobject),INTENT(inout) :: shpobj     ! shape object to simplify
    integer :: cii,oii                          ! start end vertice index
    real :: eps                                 ! tolerance in km
    !
    integer :: ii
    real :: pst
    INTEGER                                 :: nbp
    REAL(kind=8),DIMENSION(:,:),POINTER     :: points
    LOGICAL,DIMENSION(:),POINTER            :: valid
    real :: re=6371.0D0 ! earth radius in km
    logical :: meet
    !
    ! For the ramer-douglas-peucker algorithm, the last point must be different
    ! of the first point to avoid a division by 0 in 'perpendicularDistance()'
    ! nbp = 10
    ! allocate(points(2,nbp))
    meet=(shpobj%padfx(cii).eq.shpobj%padfx(oii) .and. &
         & shpobj%padfy(cii).eq.shpobj%padfy(oii))
    if (meet) then
       nbp=oii-cii
    else
       nbp=oii-cii-1
    end if
    if (nbp.le.3) return
    allocate(points(2,nbp),valid(nbp))
    do ii=1,nbp
       points(1,ii)=shape_degtor(shpobj%padfy(ii))*re                      ! latitude
       points(2,ii)=shape_degtor(shpobj%padfx(ii))*re*shape_cosdeg(shpobj%padfy(ii))  ! longitude
       valid(ii)=.true.
    end do
    call shape_DouglasPeuckerRecursive(points, 1, nbp, eps, valid)
    do ii=cii,nbp+cii-1
       shpobj%valid(ii)=valid(ii-cii+1)
    end do
    shpobj%valid(cii)=.true.
    if (meet) shpobj%valid(oii)=.true.
    !call shape_DouglasPeuckerIteratif(points, eps, valid)
    deallocate( points )
    deallocate( valid )
    return
  end subroutine shape_simplify
  
  logical function shape_inside(plon,plat,minlon,minlat,maxlon,maxlat)
    implicit none
    real plon,plat,minlon,minlat,maxlon,maxlat
    shape_inside= (plon.ge.minlon.and. &
         & plon.le.maxlon.and. &
         & plat.ge.minlat.and. &
         & plat.le.maxlat)
    return
  end function shape_inside
  
  real function shape_getDistBox(pos,mimi,mima,mami,mama)
    implicit none
    real pos(3),mimi(3),mima(3),mami(3),mama(3)
    real :: d1,d2,d3,d4
    d1=shape_getDist(pos,mimi,mima)
    d2=shape_getDist(pos,mima,mama)
    d3=shape_getDist(pos,mama,mami)
    d4=shape_getDist(pos,mami,mimi)
    shape_getDistBox=min(d1,d2,d3,d4)
    return
  end function shape_getDistBox
  real function shape_getDist(p,a,b)
    ! get distance between point (P) and segment (between A and B)
    implicit none
    real :: p(3)
    real :: a(3)
    real :: b(3)
    real :: ab(3) ! A-B
    real :: ap(3) ! A-P
    real :: bp(3) ! B-P
    real :: dab,dd,r
    call shape_subtract(b,a,ab)
    call shape_subtract(p,a,ap)
    dab=dot_product(ab,ab)
    if (dab.lt.1.0D-10) then ! A and B are identical
       shape_getDist=dsqrt(dot_product(ap,ap))
    else
       dd=dot_product(ab,ap)
       r=dd/dab
       if (r.le.0.0D0) then ! a is closest
          shape_getDist=sqrt(dot_product(ap,ap))
       else if (r.ge.1.0D0) then ! b=closest
          call shape_subtract(p,b,bp)
          shape_getDist=sqrt(dot_product(bp,bp))
       else ! between a and b
          shape_getDist=sqrt(max(0.0D0,dot_product(ap,ap) - dab*(R**2)))
       end if
    end if
    return
  end function shape_getDist
  subroutine shape_lonlat2pos(lon,lat,pos)
    implicit none
    real :: lat,lon,pos(3),rr
    pos(3) = shape_sindeg(lat)
    rr     = shape_cosdeg(lat)
    pos(1) = rr*shape_cosdeg(lon)
    pos(2) = rr*shape_sindeg(lon)
    return
  end subroutine shape_lonlat2pos

  subroutine shape_cross(a, b, c)
    real, dimension(3), intent(in) :: a, b
    real, dimension(3), intent(out) :: c
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
    return
  end subroutine shape_cross
  
  subroutine shape_divide(a, b, c)
    real, dimension(3), intent(in) :: a
    real, intent(in) :: b
    real, dimension(3), intent(out) :: c
    c(1) = a(1) /b
    c(2) = a(2) /b
    c(3) = a(3) /b
    return
  end subroutine shape_divide

  subroutine shape_multiply(a, b, c)
    real, dimension(3), intent(in) :: a
    real, intent(in) :: b
    real, dimension(3), intent(out) :: c
    c(1) = a(1) * b
    c(2) = a(2) * b
    c(3) = a(3) * b
    return
  end subroutine shape_multiply
  
  real function shape_dot(a, b)
    real, dimension(3), intent(in) :: a
    real, dimension(3), intent(in) :: b
    shape_dot = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
    return
  end function shape_dot
  
  subroutine shape_subtract(a, b, c)
    real, dimension(3), intent(in) :: a, b
    real, dimension(3), intent(out) :: c
    c(1) = a(1) - b(1)
    c(2) = a(2) - b(2)
    c(3) = a(3) - b(3)
    return
  end subroutine shape_subtract
  
  real function shape_getAngle(apos,bpos)
    implicit none
    real :: apos(3),bpos(3),dd ! assuming normalised vectors
    dd=dot_product(apos,bpos)
    shape_getAngle=shape_acosdeg(dd)
    return
  end function shape_getAngle
  
  character*20 function string20(pos)
    real :: pos(3)
    write(string20,'(F6.3,X,F6.3,X,F6.3)') pos
  end function string20
    
  real function shape_degtor(x)
    implicit none
    real :: x
    real :: pi
    parameter (pi=3.14159265359)
    shape_degtor=x*pi/180.
  end function shape_degtor

  real function shape_rtodeg(x)
    implicit none
    real :: x
    real :: pi
    parameter (pi=3.14159265359)
    shape_rtodeg=x*180./pi
  end function shape_rtodeg

  real function shape_sindeg(x)
    implicit none
    real :: x
    shape_sindeg=sin(shape_degtor(x))
  end function shape_sindeg

  real function shape_cosdeg(x)
    implicit none
    real :: x
    shape_cosdeg=cos(shape_degtor(x))
  end function shape_cosdeg

  real function shape_tandeg(x)
    implicit none
    real :: x
    shape_tandeg=tan(shape_degtor(x))
  end function shape_tandeg

  real function shape_asindeg(x)
    implicit none
    real :: x
    shape_asindeg=shape_rtodeg(asin(x))
  end function shape_asindeg

  real function shape_acosdeg(x)
    implicit none
    real :: x
    shape_acosdeg=shape_rtodeg(acos(x))
  end function shape_acosdeg

  real function shape_atandeg(x)
    implicit none
    real :: x
    shape_atandeg=shape_rtodeg(atan(x))
  end function shape_atandeg

  real function atan2deg(y,x)
    implicit none
    real :: y,x
    atan2deg=shape_rtodeg(atan2(y,x))
  end function atan2deg

  ! !
  ! real function shape_degtor(x)
  !   implicit none
  !   real(kind=8),INTENT(IN)  :: x
  !   real pi
  !   parameter (pi=3.14159265359)
  !   shape_degtor=x*pi/180.
  ! end function shape_degtor

  ! real function shape_rtodeg(x)
  !   implicit none
  !   real(kind=8),INTENT(IN)  :: x
  !   real pi
  !   parameter (pi=3.14159265359)
  !   shape_rtodeg=x*180./pi
  ! end function shape_rtodeg
  ! !
  ! real function shape_sindeg(x)
  !   implicit none
  !   real(kind=8),INTENT(IN)  :: x
  !   shape_sindeg=sin(shape_degtor(x))
  ! end function shape_sindeg

  ! real function shape_cosdeg(x)
  !   implicit none
  !   real(kind=8),INTENT(IN)  :: x
  !   shape_cosdeg=cos(shape_degtor(x))
  ! end function shape_cosdeg

END MODULE shape

