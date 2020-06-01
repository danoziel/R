* import_lockdown_effect_survey.do
*
* 	Imports and aggregates "Lockdown effect survey" (ID: lockdown_effect_survey) data.
*
*	Inputs:  "Lockdown effect survey_WIDE.csv"
*	Outputs: "Lockdown effect survey.dta"
*
*	Output by SurveyCTO May 7, 2020 5:54 AM.

* initialize Stata
clear all
set more off
set mem 100m

* initialize workflow-specific parameters
*	Set overwrite_old_data to 1 if you use the review and correction
*	workflow and allow un-approving of submissions. If you do this,
*	incoming data will overwrite old data, so you won't want to make
*	changes to data in your local .dta file (such changes can be
*	overwritten with each new import).
local overwrite_old_data 0

* initialize form-specific parameters
local csvfile "Lockdown effect survey_WIDE.csv"
local dtafile "Lockdown effect survey.dta"
local corrfile "Lockdown effect survey_corrections.csv"
local note_fields1 ""
local text_fields1 "deviceid subscriberid simid devicephonenum username duration caseid regional_initiative cluster_names mandal_name interviewer_name interviewer_contact_number farmer_name farmer_mobile_number"
local text_fields2 "village_name farmer_participant_vcf_trials kind_of_trial crops_grown other_crops crop_details_count crop_name_* other_specify_crop_* milk_production daily_needs_market access_doctor"
local text_fields3 "food_grains_stock_month lost_job_member celebrations_stopped functions_happened_lockdown medical_checkup additional_information_family instanceid"
local date_fields1 "interview_date planted_date_*"
local datetime_fields1 "submissiondate starttime endtime"

disp
disp "Starting import of: `csvfile'"
disp

* import data from primary .csv file
insheet using "`csvfile'", names clear

* drop extra table-list columns
cap drop reserved_name_for_field_*
cap drop generated_table_list_lab*

* continue only if there's at least one row of data to import
if _N>0 {
	* drop note fields (since they don't contain any real data)
	forvalues i = 1/100 {
		if "`note_fields`i''" ~= "" {
			drop `note_fields`i''
		}
	}
	
	* format date and date/time fields
	forvalues i = 1/100 {
		if "`datetime_fields`i''" ~= "" {
			foreach dtvarlist in `datetime_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=clock(`tempdtvar',"DMYhms",2025)
						* automatically try without seconds, just in case
						cap replace `dtvar'=clock(`tempdtvar',"DMYhm",2025) if `dtvar'==. & `tempdtvar'~=""
						format %tc `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
		if "`date_fields`i''" ~= "" {
			foreach dtvarlist in `date_fields`i'' {
				cap unab dtvarlist : `dtvarlist'
				if _rc==0 {
					foreach dtvar in `dtvarlist' {
						tempvar tempdtvar
						rename `dtvar' `tempdtvar'
						gen double `dtvar'=.
						cap replace `dtvar'=date(`tempdtvar',"DMY",2025)
						format %td `dtvar'
						drop `tempdtvar'
					}
				}
			}
		}
	}

	* ensure that text fields are always imported as strings (with "" for missing values)
	* (note that we treat "calculate" fields as text; you can destring later if you wish)
	tempvar ismissingvar
	quietly: gen `ismissingvar'=.
	forvalues i = 1/100 {
		if "`text_fields`i''" ~= "" {
			foreach svarlist in `text_fields`i'' {
				cap unab svarlist : `svarlist'
				if _rc==0 {
					foreach stringvar in `svarlist' {
						quietly: replace `ismissingvar'=.
						quietly: cap replace `ismissingvar'=1 if `stringvar'==.
						cap tostring `stringvar', format(%100.0g) replace
						cap replace `stringvar'="" if `ismissingvar'==1
					}
				}
			}
		}
	}
	quietly: drop `ismissingvar'


	* consolidate unique ID into "key" variable
	replace key=instanceid if key==""
	drop instanceid


	* label variables
	label variable key "Unique submission ID"
	cap label variable submissiondate "Date/time submitted"
	cap label variable formdef_version "Form version used on device"
	cap label variable review_status "Review status"
	cap label variable review_comments "Comments made during review"
	cap label variable review_corrections "Corrections made during review"


	label variable regional_initiative "Name of Regional Initiative"
	note regional_initiative: "Name of Regional Initiative"

	label variable cluster_names "Cluster Name"
	note cluster_names: "Cluster Name"

	label variable mandal_name "Mandal Name"
	note mandal_name: "Mandal Name"

	label variable interviewer_name "Interviewer Name"
	note interviewer_name: "Interviewer Name"

	label variable interviewer_contact_number "Interviewer Contact Number"
	note interviewer_contact_number: "Interviewer Contact Number"

	label variable interview_date "Interview Date"
	note interview_date: "Interview Date"

	label variable farmer_name "Farmer Name"
	note farmer_name: "Farmer Name"

	label variable farmer_mobile_number "Farmer Mobile Number"
	note farmer_mobile_number: "Farmer Mobile Number"

	label variable village_name "Village Name"
	note village_name: "Village Name"

	label variable farmer_participant_vcf_trials "Is the farmer participant in any of the VCF trials/activities?"
	note farmer_participant_vcf_trials: "Is the farmer participant in any of the VCF trials/activities?"

	label variable kind_of_trial "What kind of trial/activity?"
	note kind_of_trial: "What kind of trial/activity?"

	label variable crops_grown "What Crops are you growing?"
	note crops_grown: "What Crops are you growing?"

	label variable other_crops "Others (Specify)"
	note other_crops: "Others (Specify)"

	label variable milk_production "Do you have milk production?"
	note milk_production: "Do you have milk production?"

	label variable milch_animals "How many milch animals do you have?"
	note milch_animals: "How many milch animals do you have?"

	label variable milk_sold_daily "How many litres of milk is sold daily?"
	note milk_sold_daily: "How many litres of milk is sold daily?"

	label variable price_milk_per_litre_before_lock "What was the average rate per litre you got before lockdown?"
	note price_milk_per_litre_before_lock: "What was the average rate per litre you got before lockdown?"

	label variable price_milk_per_litre_after_lockd "What was the average rate per litre you got after lockdown?"
	note price_milk_per_litre_after_lockd: "What was the average rate per litre you got after lockdown?"

	label variable daily_needs_market "Are you able to get all necessary daily needs from markets?"
	note daily_needs_market: "Are you able to get all necessary daily needs from markets?"

	label variable access_doctor "If required, do you have any access to qualified doctor?"
	note access_doctor: "If required, do you have any access to qualified doctor?"

	label variable food_grains_stock_month "Do you have adequate stock of food grains for a month?"
	note food_grains_stock_month: "Do you have adequate stock of food grains for a month?"

	label variable lost_job_member "Has any of your family member/relative lost job in city and has returned from ci"
	note lost_job_member: "Has any of your family member/relative lost job in city and has returned from city due to lockdown?"

	label variable lost_job_member_number "How many members?"
	note lost_job_member_number: "How many members?"

	label variable celebrations_stopped "Are celebrations/functions like marriage fully stopped in your village?"
	note celebrations_stopped: "Are celebrations/functions like marriage fully stopped in your village?"

	label variable functions_happened_lockdown "What functions happened after lockdown?"
	note functions_happened_lockdown: "What functions happened after lockdown?"

	label variable medical_checkup "Was any medical checkup of family done at GP level?"
	note medical_checkup: "Was any medical checkup of family done at GP level?"

	label variable additional_information_family "Any additional relevant information about the family from the team?"
	note additional_information_family: "Any additional relevant information about the family from the team?"



	capture {
		foreach rgvar of varlist crop_name_* {
			label variable `rgvar' "Crop Name"
			note `rgvar': "Crop Name"
		}
	}

	capture {
		foreach rgvar of varlist other_specify_crop_* {
			label variable `rgvar' "Other- Specify the name of the Crop"
			note `rgvar': "Other- Specify the name of the Crop"
		}
	}

	capture {
		foreach rgvar of varlist crop_acres_* {
			label variable `rgvar' "How many acres have you planted?"
			note `rgvar': "How many acres have you planted?"
		}
	}

	capture {
		foreach rgvar of varlist planted_date_* {
			label variable `rgvar' "When was it planted?"
			note `rgvar': "When was it planted?"
		}
	}

	capture {
		foreach rgvar of varlist sold_before_lockdown_quintals_* {
			label variable `rgvar' "How much was sold by you before lockdown? (In Quintals)"
			note `rgvar': "How much was sold by you before lockdown? (In Quintals)"
		}
	}

	capture {
		foreach rgvar of varlist price_before_lockdown_rs_per_kg_* {
			label variable `rgvar' "How much was the average price per kg in the village before lockdown? (Rs/Kg)"
			note `rgvar': "How much was the average price per kg in the village before lockdown? (Rs/Kg)"
		}
	}

	capture {
		foreach rgvar of varlist price_after_lockdown_rs_per_kg_* {
			label variable `rgvar' "What is the average price per Kg now in the village after lockdown ? (Rs/Kg)"
			note `rgvar': "What is the average price per Kg now in the village after lockdown ? (Rs/Kg)"
		}
	}

	capture {
		foreach rgvar of varlist crop_yet_to_harvest_apr_2020_qui_* {
			label variable `rgvar' "How much crop is yet to be harvested in the month of April 2020? (In Quintals)"
			note `rgvar': "How much crop is yet to be harvested in the month of April 2020? (In Quintals)"
		}
	}




	* append old, previously-imported data (if any)
	cap confirm file "`dtafile'"
	if _rc == 0 {
		* mark all new data before merging with old data
		gen new_data_row=1
		
		* pull in old data
		append using "`dtafile'"
		
		* drop duplicates in favor of old, previously-imported data if overwrite_old_data is 0
		* (alternatively drop in favor of new data if overwrite_old_data is 1)
		sort key
		by key: gen num_for_key = _N
		drop if num_for_key > 1 & ((`overwrite_old_data' == 0 & new_data_row == 1) | (`overwrite_old_data' == 1 & new_data_row ~= 1))
		drop num_for_key

		* drop new-data flag
		drop new_data_row
	}
	
	* save data to Stata format
	save "`dtafile'", replace

	* show codebook and notes
	codebook
	notes list
}

disp
disp "Finished import of: `csvfile'"
disp

* OPTIONAL: LOCALLY-APPLIED STATA CORRECTIONS
*
* Rather than using SurveyCTO's review and correction workflow, the code below can apply a list of corrections
* listed in a local .csv file. Feel free to use, ignore, or delete this code.
*
*   Corrections file path and filename:  Lockdown effect survey_corrections.csv
*
*   Corrections file columns (in order): key, fieldname, value, notes

capture confirm file "`corrfile'"
if _rc==0 {
	disp
	disp "Starting application of corrections in: `corrfile'"
	disp

	* save primary data in memory
	preserve

	* load corrections
	insheet using "`corrfile'", names clear
	
	if _N>0 {
		* number all rows (with +1 offset so that it matches row numbers in Excel)
		gen rownum=_n+1
		
		* drop notes field (for information only)
		drop notes
		
		* make sure that all values are in string format to start
		gen origvalue=value
		tostring value, format(%100.0g) replace
		cap replace value="" if origvalue==.
		drop origvalue
		replace value=trim(value)
		
		* correct field names to match Stata field names (lowercase, drop -'s and .'s)
		replace fieldname=lower(subinstr(subinstr(fieldname,"-","",.),".","",.))
		
		* format date and date/time fields (taking account of possible wildcards for repeat groups)
		forvalues i = 1/100 {
			if "`datetime_fields`i''" ~= "" {
				foreach dtvar in `datetime_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						gen origvalue=value
						replace value=string(clock(value,"DMYhms",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
						* allow for cases where seconds haven't been specified
						replace value=string(clock(origvalue,"DMYhm",2025),"%25.0g") if strmatch(fieldname,"`dtvar'") & value=="." & origvalue~="."
						drop origvalue
					}
				}
			}
			if "`date_fields`i''" ~= "" {
				foreach dtvar in `date_fields`i'' {
					* skip fields that aren't yet in the data
					cap unab dtvarignore : `dtvar'
					if _rc==0 {
						replace value=string(clock(value,"DMY",2025),"%25.0g") if strmatch(fieldname,"`dtvar'")
					}
				}
			}
		}

		* write out a temp file with the commands necessary to apply each correction
		tempfile tempdo
		file open dofile using "`tempdo'", write replace
		local N = _N
		forvalues i = 1/`N' {
			local fieldnameval=fieldname[`i']
			local valueval=value[`i']
			local keyval=key[`i']
			local rownumval=rownum[`i']
			file write dofile `"cap replace `fieldnameval'="`valueval'" if key=="`keyval'""' _n
			file write dofile `"if _rc ~= 0 {"' _n
			if "`valueval'" == "" {
				file write dofile _tab `"cap replace `fieldnameval'=. if key=="`keyval'""' _n
			}
			else {
				file write dofile _tab `"cap replace `fieldnameval'=`valueval' if key=="`keyval'""' _n
			}
			file write dofile _tab `"if _rc ~= 0 {"' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab _tab `"disp "CAN'T APPLY CORRECTION IN ROW #`rownumval'""' _n
			file write dofile _tab _tab `"disp"' _n
			file write dofile _tab `"}"' _n
			file write dofile `"}"' _n
		}
		file close dofile
	
		* restore primary data
		restore
		
		* execute the .do file to actually apply all corrections
		do "`tempdo'"

		* re-save data
		save "`dtafile'", replace
	}
	else {
		* restore primary data		
		restore
	}

	disp
	disp "Finished applying corrections in: `corrfile'"
	disp
}
