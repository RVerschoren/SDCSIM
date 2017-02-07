# SDCSIM
SSD simulator based on the d-Choices GCA

## Simulating Synthetic Workloads

The executables must be called as follows:

`./Sim<method>.exe <number_of_pages_per_block> <d> <rho> <f> <r> <init_run> <number_of_runs> <number_of_blocks> <maximum_number_of_PE_cycles>`

For example, to simulate a system using the hot/cold Rosenblum data model and the hot/cold write frontier (HCWF) mode with:
    * b=32 pages per block,
    * dChoices GCA with d=10,
    * load rho=0.90 (equivalent to an over-provisioning factor of 0.10),
    * hot fraction f=0.01,
    * hot data request ratio r=0.99,
    * 20 runs starting from 1,
    * N=10000 blocks with maximum number of erasures 5000,
    * initialized by associating random physical pages to all logical pages,
the following command can be executed:
`./SimHCWF.exe 32 10 0.90 0.01 0.99 1 20 10000 5000  T`


## Simulating a trace-based workload

The syntax is a little bit different when simulating a trace-based workload:

`./Sim<method>.exe <number_of_pages_per_block> <d> <rho> <f> <init_run> <number_of_runs> <number_of_blocks> <maximum_number_of_PE_cycles> <number_of_requests> <tracefile> <init_random>`

For example, to simulate a system using a trace and the double write frontier (DWF) mode with:
    * b=32 pages per block,
    * dChoices GCA with d=10,
    * load rho=0.86 (equivalent to an over-provisioning factor of 0.14),
    * hot fraction f=0.10,
    * a trace file (in CSV file format) called trace.csv containing 250368 requests,
    * 15 runs starting from 5,
    * N=10000 blocks with maximum number of erasures 5000,
    * initialized empty, filled with requests from the trace,
the following command can be executed:
`./SimHCWF.exe 32 10 0.86 0.10 5 15 10000 5000 250368 trace.csv F`

The first 4 letters of the trace filename are used as ID and will be incorporated in the output.
Each line in the trace file must be in the format "<LPN>,<command>", where <LPN> represents the requested logical page number.
The value in the <command> field can be used to differentiate between TRIM and write commands.
If this value is equal to 0 (zero), this will be interpreted as a TRIM command. All non-zero values will be interpreted as a write command.
