FC = gfortran
FFLAGS = -O2 -fopenmp -std=f2008 -fall-intrinsics -fcheck=bounds -fcheck=all

DWFEXEC = SimDWF.exe
DWFOBJS =  utils.o sim.o SimDWF.o

HCWFEXEC = SimHCWF.exe
HCWFOBJS =  utils.o sim.o SimHCWF.o

DWFTRACEEXEC = SimDWFTrace.exe
DWFTRACEOBJS =  utils.o sim.o SimDWFTrace.o

HCWFTRACEEXEC = SimHCWFTrace.exe
HCWFTRACEOBJS =  utils.o sim.o SimHCWFTrace.o


.PHONY: all
all: $(HCWFEXEC) $(DWFEXEC) $(DWFTRACEEXEC) $(HCWFTRACEEXEC)

.SUFFIXES: .f90 .f08 .o .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

$(DWFEXEC): $(DWFOBJS)
	$(FC) $(FFLAGS) $(DWFOBJS) -o $@

$(DWFTRACEEXEC): $(DWFTRACEOBJS)
	$(FC) $(FFLAGS) $(DWFTRACEOBJS) -o $@

$(HCWFEXEC): $(HCWFOBJS)
	$(FC) $(FFLAGS) $(HCWFOBJS) -o $@

$(HCWFTRACEEXEC): $(HCWFTRACEOBJS)
	$(FC) $(FFLAGS) $(HCWFTRACEOBJS) -o $@

.PHONY: clean
clean:
	find . -name '*.o' -delete
	find . -name '*.mod' -delete
	find . -name '*.exe'  -delete
	find . -name '*~'  -delete
