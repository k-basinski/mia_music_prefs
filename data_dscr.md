# data_mia - dataset description

Each row in the dataset contains data on one participant. This format does not exactly adhere to Tidy Data rules as stated [here](https://www.jstatsoft.org/article/view/v059i10/v59i10.pdf), but we believe this format is easier to understand.

Most of the variables are prefixed with a `A_`, `V_`, `D_` or `N_`. This corresponds to the arousal, valence, depth and noise conditions respectively. `Curr` stands for "current" and refers to baseline values (i.e. of blood pressure) before the experiment. For example the variable `D_PainMax` refers to maximal pain reported by a participant in the depth condition.

## Variable coding
- `PainMax` - maximal pain, 11-point numerical rating scale (NRS)
- `PainAvg` - average pain, 11-point NRS
- `PainContr` - pain controllability, 11-point NRS
- `PainThresh` - pain threshold, seconds
- `PainToler` - pain tolerance, seconds
- `HR` - heart rate
- `BPSys` - systolic blood pressure
- `BPDias` - diastolic blood pressure
- `order` of the trials
- `songOrder` - which song was played first during the given trial
- `ITT` - inter-trial time in seconds, `n` when non-applicable


## Other variables
- `Gender`
- `Date` of the trial
- `Age` of the participant
- `IsLefty` is the participant left-handed, 0-false, 1-true
- `Education` - 1-primary, 2-secondary, 3-higher Education
- `AVND_Order` - order of A, V, D and N trials 
