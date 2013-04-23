/*
  Program: Matched_Cohort.sas
  Purpose: To match two datasets on any number of matching variables and output 
  Author : Adam Omidpanah

  Modification Log:
    04/18/2013

  Notes: 

    Presently, this program drops unmatched clusters of data. "Ratio" is the desired matching ratio from data1
    (smaller and more interesting data group) to data2 (the larger referent group). Specifying ratio=1 gives
    1 to 1 whereas ratio=10 will match at most 10 cases in data2 for each such case in data1. Note if there 
    are an insufficient number of matched pairs, it will return a deficit group with fewer cases. One can 
    restrict to complete clusters by indicating that "_complete_=1" in a where statement.
*/

%macro matched_cohort(data1, data2, outdata, matchvars, ratio=1);
/* 1. create indicator for unique levels of matchvars in order to restrict number of obs in data2*/
data _sets_;
  set &data1 &data2;
  keep &matchvars;
proc sort data=_sets_ nodup; 
  by &matchvars;
data _sets_;
  set _sets_;
  _indexset_ = _n_;
run;

/* 2. load indicators into original data */
proc sort data=&data1 out=_data1_;
  by &matchvars;
proc sort data=&data2 out=_data2_;
  by &matchvars;
data _data1_;
  merge _data1_(in=_keeper_) _sets_;
  by &matchvars;
  if _keeper_;
data _data2_;
  merge _data2_(in=_keeper_) _sets_;
  by &matchvars;
  if _keeper_;
run;

/* 3. merge frequencies of indexed sets in primary data to secondary data and restrict number of observations */
proc freq data=_data1_;
  tables _indexset_ / out=_count_;
proc sort data=_count_(keep = _indexset_ count rename=(count=_count_)); 
  by _indexset_;
proc sort data=_data2_;
  by _indexset_;
data _data2_;
  merge _data2_ _count_;
  by _indexset_;
  retain _numobs_;
  if first._indexset_ then _numobs_ = 0;
  _numobs_ = _numobs_ + 1;
  if (_numobs_ / _count_) <= &ratio then output;
run;

/* 4. Stack data sorted by _indexset_, dropped unmatched sets */
data &outdata;
  set _data1_ _data2_;
  if _numobs_ > 0 and _count_ ^= .;
run;

/* 5. Create complete grouping indicator */
proc sort data=&outdata; 
  by _indexset_; 
data _tmp_;
  set &outdata;
  by _indexset_;
  if last._indexset_ then do;
    _complete_ = (_numobs_ / _count_ = &ratio);
  output;
  end;
data &outdata;
  merge &outdata _tmp_;
  by _indexset_;
  drop _numobs_ _count_;
run;

/* 5. Clean workspace */
proc datasets library=work;
  delete _data1_ _data2_ _sets_ _count_ _tmp_;
quit;
%mend;
