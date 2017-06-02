FC = gfortran
FFLAGS = -O2 -fopenmp -std=f2008 -fall-intrinsics -fcheck=bounds -fcheck=all

CC = g++
CFLAGS = -O2 -std=c++11

HCWFUTIL = utils.o gca.o hcwfsim.o
DWFUTIL = utils.o gca.o dwfsim.o

DWFEXEC = SimDWF.exe
DWFOBJS =  $(DWFUTIL) SimDWF.o

HCWFEXEC = SimHCWF.exe
HCWFOBJS =  $(HCWFUTIL) SimHCWF.o

DWFTRACEEXEC = SimDWFTrace.exe
DWFTRACEOBJS =  $(DWFUTIL) SimDWFTrace.o

COLDTRACEEXEC = SimCOLDTrace.exe
COLDTRACEOBJS =  $(HCWFUTIL) SimCOLDTrace.o

HCWFTRACEEXEC = SimHCWFTrace.exe
HCWFTRACEOBJS =  $(HCWFUTIL) SimHCWFTrace.o

ORACLETRACEEXEC = SimORACLETrace.exe
ORACLETRACEOBJS = $(HCWFUTIL) SimHCORACLETrace.o

PREPROCTRACEEXEC = oracle.exe
PREPROCTRACEOBJS =  oracle.o



.PHONY: all
all: $(HCWFEXEC) $(DWFEXEC) $(DWFTRACEEXEC) $(HCWFTRACEEXEC) $(COLDTRACEEXEC) $(ORACLETRACEEXEC) $(PREPROCTRACEEXEC)

.SUFFIXES: .f90 .o .mod .cxx

.f90.o:
	$(FC) $(FFLAGS) -c $<

.cxx.o:
	$(CC) $(CFLAGS) -c $<

$(DWFEXEC): $(DWFOBJS)
	$(FC) $(FFLAGS) $(DWFOBJS) -o $@

$(DWFTRACEEXEC): $(DWFTRACEOBJS)
	$(FC) $(FFLAGS) $(DWFTRACEOBJS) -o $@

$(HCWFEXEC): $(HCWFOBJS)
	$(FC) $(FFLAGS) $(HCWFOBJS) -o $@

$(HCWFTRACEEXEC): $(HCWFTRACEOBJS)
	$(FC) $(FFLAGS) $(HCWFTRACEOBJS) -o $@
	
$(COLDTRACEEXEC): $(COLDTRACEOBJS)
	$(FC) $(FFLAGS) $(COLDTRACEOBJS) -o $@

$(ORACLETRACEEXEC): $(ORACLETRACEOBJS)
	$(FC) $(FFLAGS) $(ORACLETRACEOBJS) -o $@

$(PREPROCTRACEEXEC): $(PREPROCTRACEOBJS)
	$(CC) $(CFLAGS) $(PREPROCTRACEOBJS) -o $@

.PHONY: clean
clean:
	find . -name '*.o' -delete
	find . -name '*.mod' -delete
	find . -name '*.exe'  -delete
	find . -name '*~'  -delete
