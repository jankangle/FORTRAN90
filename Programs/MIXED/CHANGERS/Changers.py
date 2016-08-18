# Title: GUI for changing basis set formats (Python 2.7.6)
# Date Created: Aug 17th, 2016
# Date Last Modified: Aug 18th, 2016
# Version: 1.0
#
# Author: Jonathan Kung
# University of Calgary
# Purpose: Create a simple GUI to allow users to easily change basis set formats from one program to another

### Import Libraries ###
from Tkinter import *
import ttk
import os
import _OK
import f90wrap.runtime
import logging

### F90Wrap modules ###
class Vars(f90wrap.runtime.FortranModule):
    """
    Module vars
    
    
    Defined at TEST.f90 lines 1-11
    
    """
    class Gbs(f90wrap.runtime.FortranDerivedType):
        """
        Type(name=gbs)
        
        
        Defined at TEST.f90 lines 3-6
        
        """
        def __init__(self, handle=None):
            """
            self = Gbs()
            
            
            Defined at TEST.f90 lines 3-6
            
            
            Returns
            -------
            this : Gbs
            	Object to be constructed
            
            
            Automatically generated constructor for gbs
            """
            f90wrap.runtime.FortranDerivedType.__init__(self)
            self._handle = _OK.f90wrap_gbs_initialise()
        
        def __del__(self):
            """
            Destructor for class Gbs
            
            
            Defined at TEST.f90 lines 3-6
            
            Parameters
            ----------
            this : Gbs
            	Object to be destructed
            
            
            Automatically generated destructor for gbs
            """
            if self._alloc:
                _OK.f90wrap_gbs_finalise(this=self._handle)
        
        @property
        def lmax(self):
            """
            Element lmax ftype=character(len=2) pytype=str
            
            
            Defined at TEST.f90 line 4
            
            """
            return _OK.f90wrap_gbs__get__lmax(self._handle)
        
        @lmax.setter
        def lmax(self, lmax):
            _OK.f90wrap_gbs__set__lmax(self._handle, lmax)
        
        @property
        def ngbs(self):
            """
            Element ngbs ftype=integer  pytype=int
            
            
            Defined at TEST.f90 line 5
            
            """
            return _OK.f90wrap_gbs__get__ngbs(self._handle)
        
        @ngbs.setter
        def ngbs(self, ngbs):
            _OK.f90wrap_gbs__set__ngbs(self._handle, ngbs)
        
        @property
        def fgbs(self):
            """
            Element fgbs ftype=real pytype=float
            
            
            Defined at TEST.f90 line 6
            
            """
            array_ndim, array_type, array_shape, array_handle = \
                _OK.f90wrap_gbs__array__fgbs(self._handle)
            if array_handle in self._arrays:
                fgbs = self._arrays[array_handle]
            else:
                fgbs = f90wrap.runtime.get_array(f90wrap.runtime.sizeof_fortran_t,
                                        self._handle,
                                        _OK.f90wrap_gbs__array__fgbs)
                self._arrays[array_handle] = fgbs
            return fgbs
        
        @fgbs.setter
        def fgbs(self, fgbs):
            self.fgbs[...] = fgbs
        
        def __str__(self):
            ret = ['<gbs>{\n']
            ret.append('    lmax : ')
            ret.append(repr(self.lmax))
            ret.append(',\n    ngbs : ')
            ret.append(repr(self.ngbs))
            ret.append(',\n    fgbs : ')
            ret.append(repr(self.fgbs))
            ret.append('}')
            return ''.join(ret)
        
        _dt_array_initialisers = []
        
    @property
    def nblk(self):
        """
        Element nblk ftype=integer  pytype=int
        
        
        Defined at TEST.f90 line 8
        
        """
        return _OK.f90wrap_vars__get__nblk()
    
    @nblk.setter
    def nblk(self, nblk):
        _OK.f90wrap_vars__set__nblk(nblk)
    
    @property
    def size(self):
        """
        Element size ftype=integer  pytype=int
        
        
        Defined at TEST.f90 line 8
        
        """
        return _OK.f90wrap_vars__get__size()
    
    @size.setter
    def size(self, size):
        _OK.f90wrap_vars__set__size(size)
    
    @property
    def atom(self):
        """
        Element atom ftype=character(len=15) pytype=str
        
        
        Defined at TEST.f90 line 10
        
        """
        return _OK.f90wrap_vars__get__atom()
    
    @atom.setter
    def atom(self, atom):
        _OK.f90wrap_vars__set__atom(atom)
    
    @property
    def newatm(self):
        """
        Element newatm ftype=character(len=15) pytype=str
        
        
        Defined at TEST.f90 line 10
        
        """
        return _OK.f90wrap_vars__get__newatm()
    
    @newatm.setter
    def newatm(self, newatm):
        _OK.f90wrap_vars__set__newatm(newatm)
    
    def __str__(self):
        ret = ['<vars>{\n']
        ret.append('    nblk : ')
        ret.append(repr(self.nblk))
        ret.append(',\n    size : ')
        ret.append(repr(self.size))
        ret.append(',\n    atom : ')
        ret.append(repr(self.atom))
        ret.append(',\n    newatm : ')
        ret.append(repr(self.newatm))
        ret.append('}')
        return ''.join(ret)
    
    _dt_array_initialisers = []
    

vars = Vars()

def angchk(ang):
    """
    check = angchk(ang)
    
    
    Defined at TEST.f90 lines 13-22
    
    Parameters
    ----------
    ang : str
    
    Returns
    -------
    check : bool
    
    """
    check = _OK.f90wrap_angchk(ang=ang)
    return check

def lprint(ang, lblk, ndim, i, j):
    """
    lprint(ang, lblk, ndim, i, j)
    
    
    Defined at TEST.f90 lines 24-53
    
    Parameters
    ----------
    ang : str
    lblk : int
    ndim : int
    i : int
    j : int
    
    """
    _OK.f90wrap_lprint(ang=ang, lblk=lblk, ndim=ndim, i=i, j=j)

def angmom(ang):
    """
    m = angmom(ang)
    
    
    Defined at TEST.f90 lines 55-71
    
    Parameters
    ----------
    ang : str
    
    Returns
    -------
    m : int
    
    """
    m = _OK.f90wrap_angmom(ang=ang)
    return m

def revangmom(m):
    """
    ang = revangmom(m)
    
    
    Defined at TEST.f90 lines 73-87
    
    Parameters
    ----------
    m : int
    
    Returns
    -------
    ang : str
    
    """
    ang = _OK.f90wrap_revangmom(m=m)
    return ang

def atmnmtrunc(name):
    """
    atmnmtrunc(name)
    
    
    Defined at TEST.f90 lines 89-102
    
    Parameters
    ----------
    name : str
    
    """
    _OK.f90wrap_atmnmtrunc(name=name)

def revatmnmtrunc(name):
    """
    revatmnmtrunc(name)
    
    
    Defined at TEST.f90 lines 104-117
    
    Parameters
    ----------
    name : str
    
    """
    _OK.f90wrap_revatmnmtrunc(name=name)

def nblkcnt_nwc():
    """
    count = nblkcnt_nwc()
    
    
    Defined at TEST.f90 lines 119-136
    
    
    Returns
    -------
    count : int
    
    """
    count = _OK.f90wrap_nblkcnt_nwc()
    return count

def read_file_gam_gen(bsize, finp):
    """
    read_file_gam_gen(bsize, finp)
    
    
    Defined at TEST.f90 lines 138-186
    
    Parameters
    ----------
    bsize : int
    finp : str
    
    """
    _OK.f90wrap_read_file_gam_gen(bsize=bsize, finp=finp)

def read_file_gau_gen(bsize, finp):
    """
    read_file_gau_gen(bsize, finp)
    
    
    Defined at TEST.f90 lines 188-236
    
    Parameters
    ----------
    bsize : int
    finp : str
    
    """
    _OK.f90wrap_read_file_gau_gen(bsize=bsize, finp=finp)

def read_file_nwc_gen(bsize, finp):
    """
    read_file_nwc_gen(bsize, finp)
    
    
    Defined at TEST.f90 lines 238-292
    
    Parameters
    ----------
    bsize : int
    finp : str
    
    """
    _OK.f90wrap_read_file_nwc_gen(bsize=bsize, finp=finp)

def read_file_dem_gen(bsize, finp):
    """
    read_file_dem_gen(bsize, finp)
    
    
    Defined at TEST.f90 lines 294-327
    
    Parameters
    ----------
    bsize : int
    finp : str
    
    """
    _OK.f90wrap_read_file_dem_gen(bsize=bsize, finp=finp)

def read_file_gen_gam(finp, output):
    """
    read_file_gen_gam(finp, output)
    
    
    Defined at TEST.f90 lines 329-361
    
    Parameters
    ----------
    finp : str
    output : str
    
    """
    _OK.f90wrap_read_file_gen_gam(finp=finp, output=output)

def read_file_gen_gau(finp, output):
    """
    read_file_gen_gau(finp, output)
    
    
    Defined at TEST.f90 lines 363-392
    
    Parameters
    ----------
    finp : str
    output : str
    
    """
    _OK.f90wrap_read_file_gen_gau(finp=finp, output=output)

def read_file_gen_nwc(finp, output):
    """
    read_file_gen_nwc(finp, output)
    
    
    Defined at TEST.f90 lines 394-422
    
    Parameters
    ----------
    finp : str
    output : str
    
    """
    _OK.f90wrap_read_file_gen_nwc(finp=finp, output=output)

def read_file_gen_dem(finp, output):
    """
    read_file_gen_dem(finp, output)
    
    
    Defined at TEST.f90 lines 424-472
    
    Parameters
    ----------
    finp : str
    output : str
    
    """
    _OK.f90wrap_read_file_gen_dem(finp=finp, output=output)
### End F90wrap modules ###

### Tk Widget as functions ###
# local = Which window the widget is on
# text_lbl = The text displayed on the widget
# nwidth = the width of the widget
# comms = the command associated with the widget when activated
# txtvar = The variable associated with the input of text
# var = variable associated with the on/off state of the widget
# val = the list of values used in the combobox

def Labels(local,text_lbl,nwidth=10):
    label = ttk.Label(local, text=text_lbl, width=nwidth, anchor="center")
    return label

def Buttons(local,text_lbl,comms,nwidth=10):
    button = ttk.Button(local, text=text_lbl, width=nwidth,command=comms)
    return button

def Comboboxs(local,val,nwidth=10,st="normal"):
    combobox = ttk.Combobox(local, values=val, width=nwidth,state=st)
    return combobox

def Entrys(local,txtvar,nwidth=10):
    entry = ttk.Entry(local,width=nwidth, textvariable=txtvar)
    return entry
### Popup window definition ###
# ptitle = title of the popup
# msg_txt = text within the popup
def popup(ptitle,msg_txt):
  top = Toplevel()
  top.title(ptitle)
  msg = Message(top, text=msg_txt)
  msg.pack()
  Tbutton = Buttons(top,"Exit",top.destroy)
  Tbutton.pack()

### Defining the main window ###
def main_window(frame):
    ### Getting variables from main window ### 
    def getvars(*args):
        _initform = Combobox1.get()
        _finp = Entry1.get()
        _finform = Combobox2.get()
        _output = Entry2.get()
	_numatm = Entry3.get()
        ### Executing F90 subroutines to change input ###
	def changeup(*args):
            if _initform == "GAMESS(US)":
                _OK.f90wrap_read_file_gam_gen(_numatm,_finp)
	        if _finform == "deMon2k":
		    _OK.f90wrap_read_file_gen_dem(_finp,_output)   
                elif _finform == "NWChem":
                    _OK.f90wrap_read_file_gen_nwc(_finp,_output)
                elif _finform == "Gaussian":
                    _OK.f90wrap_read_file_gen_gau(_finp,_output)
                elif _finform == "GAMESS(US)":
		    popup("Error","Same Initial and Final format selected")

            if _initform == "Gaussian":
		_OK.f90wrap_read_file_gau_gen(_numatm,_finp)
		if _finform == "deMon2k":
                    _OK.f90wrap_read_file_gen_dem(_finp,_output)
                elif _finform == "NWChem":
                    _OK.f90wrap_read_file_gen_nwc(_finp,_output)
                elif _finform == "Gaussian":
                    popup("Error","Same Initial and Final format selected")
                elif _finform == "GAMESS(US)":
		    _OK.f90wrap_read_file_gen_gam(_finp,_output)

	    if _initform == "NWChem":
		_OK.f90wrap_read_file_nwc_gen(_numatm,_finp)
		if _finform == "deMon2k":
                    _OK.f90wrap_read_file_gen_dem(_finp,_output)
                elif _finform == "NWChem":
                    popup("Error","Same Initial and Final format selected")
                elif _finform == "Gaussian":
                    _OK.f90wrap_read_file_gen_gau(_finp,_output)
                elif _finform == "GAMESS(US)":
		    _OK.f90wrap_read_file_gen_gam(_finp,_output)

            if _initform == "deMon2k":
		_OK.f90wrap_read_file_dem_gen(_numatm,_finp)
		if _finform == "deMon2k":
                    popup("Error","Same Initial and Final format selected")
                elif _finform == "NWChem":
                    _OK.f90wrap_read_file_gen_nwc(_finp,_output)
                elif _finform == "Gaussian":
                    _OK.f90wrap_read_file_gen_gau(_finp,_output)
                elif _finform == "GAMESS(US)":
		    _OK.f90wrap_read_file_gen_gam(_finp,_output)
            ### Remove junk files ###
	    os.remove("INTERMEDIATE")
	    os.remove("temp")
        changeup()
    ### Initial Basis set Name/Format ###
    Label1 = Labels(frame,"Initial Basis set:",15)
    Label1.grid(column=1,row=1)
    finp_var = StringVar()
    Entry1 = Entrys(frame, finp_var,11)
    Entry1.grid(column=2,row=1)
    Label2 = Labels(frame,"Initial Format:",15)
    Label2.grid(column=3,row=1)
    init_form = ('deMon2k','NWChem','Gaussian','GAMESS(US)')
    Combobox1 = Comboboxs(frame,init_form,10,'readonly')
    Combobox1.grid(column=4,row=1)

    ### Final Basis Set Name/Format ###
    Label3 = Labels(frame, "Final Basis set name:",16)
    Label3.grid(column=1,row=2)
    file_name_var = StringVar()
    Entry2 = Entrys(frame,file_name_var,11)
    Entry2.grid(column=2,row=2)
    Label4 = Labels(frame,"Final Format:",15)
    Label4.grid(column=3,row=2)
    fin_form = ('deMon2k','NWChem','Gaussian','GAMESS(US)')
    Combobox2 = Comboboxs(frame,fin_form,10,'readonly')
    Combobox2.grid(column=4,row=2)

    ### Number of Atoms in Basis Set ###
    Label5 = Labels(frame, "Number of Atoms:",16)
    Label5.grid(column=1,row=3)
    num_atm_var = StringVar()
    Entry3 = Entrys(frame,num_atm_var)
    Entry3.grid(column=2,row=3)

    ### CREATE/Exit ###
    Button2 = Buttons(frame,"CHANGE",getvars)
    Button2.grid(column=2,row=20)
    Button3 = Buttons(frame,"Exit",frame.destroy)
    Button3.grid(column=3,row=20)

### Initialize window ###
def main():
    root = Tk()
    frame = ttk.Frame(root)
    root.title('Basis Set Changer')
    app1 = main_window(root)
    root.mainloop()

### Start program ###
if __name__ == '__main__':
    main()
