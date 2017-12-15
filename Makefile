OBJDIR = ~/myobj/td2vtkOBJ
PROGDIR = ~/mybins

PROG =	$(addprefix $(PROGDIR)/,td2vtk)

SRCS =	ass_eval.f ass_integ_val.f ass_real_val.f check_par.f ch_geo_4n.f  \
	endatinicond_aux.f endatinicond.f entdatix_coor.f entdatix_zon.f   \
	entdatlx_elem.f entdatlx_zon.f entdatnz.f error.f GetVTKCellType.f \
	interp_eznum.f interp_nznum.f ldimen.f lec_cfe.f lec_cfn.f         \
	lec_ft.f LeeDIM.f LeeGEN.f LeeGRI.f LeeINI.f leel.f LeeMSV.f       \
	LeeMxx.f LeeOBS.f LeePAR.f LeePLT.f LeePSx.f LeeTIM.f              \
	Make_File_Name.f read_par.f read_tra.f SkipLines.f src_ncard.f     \
	TD2VTK.f verify_bw.f write_arrayn.f Write_Var_Int.f                \
	Write_Var_Real.f Write_VTK_Data_Int.f Write_VTK_Data_Real.f        \
	Write_VTK_Unstructured_Grid.f Write_VTK_Unstructured_Grid_Points.f


OBJS =	$(addprefix $(OBJDIR)/,ass_eval.o ass_integ_val.o ass_real_val.o \
	check_par.o ch_geo_4n.o  \
	endatinicond_aux.o endatinicond.o entdatix_coor.o entdatix_zon.o   \
	entdatlx_elem.o entdatlx_zon.o entdatnz.o error.o GetVTKCellType.o \
	interp_eznum.o interp_nznum.o ldimen.o lec_cfe.o lec_cfn.o         \
	lec_ft.o LeeDIM.o LeeGEN.o LeeGRI.o LeeINI.o leel.o LeeMSV.o       \
	LeeMxx.o LeeOBS.o LeePAR.o LeePLT.o LeePSx.o LeeTIM.o              \
	Make_File_Name.o read_par.o read_tra.o SkipLines.o src_ncard.o     \
	TD2VTK.o verify_bw.o write_arrayn.o Write_Var_Int.o                \
	Write_Var_Real.o Write_VTK_Data_Int.o Write_VTK_Data_Real.o        \
	Write_VTK_Unstructured_Grid.o Write_VTK_Unstructured_Grid_Points.o)

LIBS =	

F90 = gfortran
#Debug
#F90FLAGS = -ggdb -fbounds-check -Wall -Wtabs -g3
#Release
F90FLAGS = -O3
#F90FLAGS = -ggdb
#LDFLAGS = -static-libgcc -static-libstdc++ -static-libgfortran
LDFLAGS = 

.SUFFIXES : .o .f .f90 .FOR

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

cleanobj:
	rm -f $(OBJS)
clean:
	rm -f $(PROG) $(OBJS)

# Regla impl’cita para pasar .f a .o (forma nueva "patern rule").
$(OBJDIR)/%.o : %.f
	$(F90) -c $(F90FLAGS) -c $< -o $@
