# ncmask
Masking program for NetCDF files used to look for extreme values.

Welcome to the `ncmask` wiki!

## Running `ncmask.bin`


The `ncmask.bin` program can be used to mask NetCDF files and report targets in the remaining data.

A simple mask is for instance the grid-points around a point in a radius of 100km. A target is for instance the highest value.

The `ncmask.bin` program is in the `nctools` module which can be loaded on the met.no ppi using

      module load nctools

The binary reads input from stdin. For instance:

      cd ncmask/work
      ncmask.bin < vindkast_meps_stasjoner.xml


## Program input

The input format is XML, and may look like this:

      <program name="ncmask" scan="fast">
       <file input="$FILE$" mask="mask/vindkast/vindkast_meps_$METHOD$_stasjoner.nc" iter="percentile" keep="1">
        <parameter name="wind_speed_of_gust_return_period_$METHOD$" fail="none">
         <report size="500">
          <key Issued="@issued@"/>
          <key Type="Stasjon"/>
          <key Station="99710"/>
          <key Place="Bjørnøya"/>
          <key alt="16"/>
          <key District=" "/>
          <key Region="Svalbard"/>
          <key Phenomenon="Vindkast"/>
          <key Variable="Gust10m"/>
          <key Unit="År"/>
          <key Model="$MNAME$"/>
          <key Output="$MOUTPUT$"/>
          <key Partner="MetNo"/>
          <key Method="$MITER$"/>
          <aux name="wind_speed_of_gust_$METHOD$"/>
          <target name="gust" macro="@maxaux@"/>
          <required name="max" fraction="1.0" group="$GROUP$"/>
          <output xml="svalbard_stasjoner_Bjørnøya_G@group@_warning.xml"/>
          <define filter="location">
            <cylinder volume="inside">
              <center lat="74.5035"  lon="18.998" width="$RESOLUTION$"/>
            </cylinder>
          </define>
          <mask filter="location"/>
         </report>
        </parameter>
        <parameter name="wind_speed_of_gust_$METHOD$" fail="none"/>
       </file>
      </program>

The resulting XML file may look like this:

      <xweather>
       <time run="2021-11-11_11-47-00.000Z" issued="2021-11-10_12-00-00.000Z" expires="2021-11-13_06-30-00.000Z"/>
       <data>
        <parameter name="wind_speed_of_gust_return_period_mb0">
         <key Issued='2021-11-10_12-00-00.000Z' Type='Stasjon' Station='99710' 
              Place='Bjørnøya' alt='16' Region='Svalbard' Phenomenon='Vindkast' 
              Variable='Gust10m' Unit='År' Model='MEPS' Output='1t' Partner='MetNo' Method='mb0' />
        </parameter>
       </data>
      </xweather>

The program has built in macros enclosed by `@`, for instance `@issued@` will be replaced by the issued dtg. Environment variables are
encosed by `$`, for instance `$FILE$` will be replaced by the contents of the environment variable `FILE`.


The program may scan one NetCDF file, whose name is specified in the `<file input="input.nc">` XML tag. If you specify the attribute `mask` the program will produce a NetCDF file that only contains the data which passed through the mask.

The parameter tag specifies that a field should be loaded from the file into the program memory.

You may request several reports when scanning an NetCDF file. Each report has a seperate mask and may produce an output XML file.
The output XML file will contain records with key-value pairs. 

Static report keys that are passed from the input file to the output and are specified using the `<key/>` tag, for instance `<key Region="Svalbard"/>`. Dynamic keys are specified using the tags `<target/>` or `<required/>`. Dynamic values are calculated from the data, for instance `<required name="max" fraction="1.0"/>` will report the max value within the mask for each time, and name it `max`. If you want the median value, use `fraction="0.5"`. Other attributes are `area` in km2  and `count` number of grid points. If you for instance specify `area="100"` you get the lowest value in the 100 km2 area with the highest values. You may group data according to their value using the group attribute, for instance `group="0,10,25"`. The group can be retrieved using the macro `@group@`. You may also load a secondary field (using `<parameter/>` tag) and refer to the secondary value using a macro `<aux name="gust" macro="@maxaux@/>`.

The mask must first be defined using the `<define/>` tag, before it can be used using the `<mask/>` tag. You may specify the following attributes when you define the mask, `filter` (use existing NetCDF file to filter data), `tolerance` (used for simplification of polygons) and `delta` in km (used for buffer-zone around polygon). Your filter may further be a `<union/>` or `<intersection/>` of other masks. A mask can be a `<cylinder/>`, `<polygon/>`, `<polyline/>`, `<duct/>`, `<value/>`, `<string/>`, `<dimension/>` or new `<define/>`.

The `<cylinder/>` tag has the attributes `volume` that can be `"inside"` or `"outside"`, and sub-tags `<center/>` with attributes `lat`, `lon`, `altitude`, `width` and `height`. 

The `<polygon/>` tag has the attributes `volume` which can be `"inside"` or `"outside"`, `simplify` which is the name of the output file that will contain the simplified polygon, `tolerance` which is the simplification error in km. 

The `<polyline/>` tag has the attributes `volume` that can be `"inside"` or `"outside"`, `simplify` which is the name of the output file that will contain the simplified polygon, `tolerance` which is the simplification error in km, and `delta` which is the buffer-zone around the line that contains the grid points that will be used.

The `<duct/>` tag has the attribute `volume` that can be `"inside"` or `"outside"`. The sub-tags are `<start/>` and `<stop/>` with attributes `lat`, `lon`, `altitude`, `width` and `height`.

The `<value/>` tag has attributes `parameter`, `min` , `max`, `value`, `tolerance`.

The `<string/>` tag has attributes `parameter`, `target` and `dimension`, as strings in NetCDF are stored as arrays with a dimension.

The `<dimension/>` tag has attributes `parameter`, `name`, `min` and `max`.


# Local installation

To install `ncmask` locally, you first have to log onto the `PPI`,

      ssh ppi-clogin-b1

The you have to download the source code from github and compile

      git clone https://github.com/FrankThomasTveter/ncmask.git
      cd ncmask
      make
      ls ncmask/*.bin

# Updating the `nctools` module on `PPI`

The `ncmask.bin` software is part of the `nctools` module. You may install a new module by creating a new module file and directory in,

      cd /modules/centos7/user-apps/nctools
      cp -rf 0.34 XXX
      # cp   ~/ncmask.bin   XXX/bin/
      cd /modules/MET/centos7/user-modules/nctools
      cp 0.34 XXX
      emacs XXX

where XXX is the new version you have created.
