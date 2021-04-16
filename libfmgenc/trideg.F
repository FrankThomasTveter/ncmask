      real function degtor(x)
      implicit none
      real x
      real pi
      parameter (pi=3.14159265359)
      degtor=x*pi/180.
      end

      real function rtodeg(x)
      implicit none
      real x
      real pi
      parameter (pi=3.14159265359)
      rtodeg=x*180./pi
      end
      
      real function sindeg(x)
      implicit none
      real x,degtor
      sindeg=sin(degtor(x))
      end

      real function cosdeg(x)
      implicit none
      real x,degtor
      cosdeg=cos(degtor(x))
      end

      real function tandeg(x)
      implicit none
      real x,degtor
      tandeg=tan(degtor(x))
      end

      real function asindeg(x)
      implicit none
      real x,rtodeg
      asindeg=rtodeg(asin(x))
      end

      real function acosdeg(x)
      implicit none
      real x,rtodeg
      acosdeg=rtodeg(acos(x))
      end

      real function atandeg(x)
      implicit none
      real x,rtodeg
      atandeg=rtodeg(atan(x))
      end

      real function atan2deg(x,y)
      implicit none
      real x,y,rtodeg
      atan2deg=rtodeg(atan2(x,y))
      end
