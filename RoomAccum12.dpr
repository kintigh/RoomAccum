program RoomAccum12;
{$Q+}
{$APPTYPE CONSOLE}
{%File 'ModelSupport\default.txvpck'}

uses
  SysUtils,
  KWKStdXE;

const {array limits}
      earliest=-2001;
      latest=3000;
      maxperiod=100;

      epsilon=0.000001; {.0001%}
      maxrooms=200000; {practical limit}
      maxstructure_life=500;

      {array indices}
      occrm=1; newrm=2; abandonThisYear=3;

      version='12.0';
      debug=false;

type  str8= string[8];

var {integer}
    current_rooms, abandoned_rooms, total_rooms, room_error, last_error,
    structure_life,save_randseed, initial_rooms, {earliest_unabandoned,
    save_earliest_unabandoned,} nperiod, iter, global_max_rooms, too_many_rooms,
    i, j, period, orig_rms0, rms0, rms, y, lines, trace, rfactor, roomdigit: integer;

    rooms,saved_rooms: array[occrm..abandonThisYear,earliest..latest] of integer;

    period_start,period_end,target_rooms,orig_target_rooms,init_rooms,midpt_rooms,
      end_rooms,dated_rooms,initial_dated, save_initial_dated, save_dated_rooms,
      early_prev, early_next, save_early_prev, save_early_next: array[0..maxperiod] of integer;

    {real}
    use_life_std,  growth_rate_increment: real;

    growth_rate,min_growth_rate,max_growth_rate: array [1..maxperiod] of real;

    {boolean}
    show_iterations, target_warn, last_period, randomize_use_life, no_go,
    iterate_rate, print_implied, first_period_run,
    writecsv, writeyear, writetxt, screenyear,
    randomize_period_1_age,gaussian_use_life,skip_to_last_year, abandon_error: boolean;

    {other}
    yearfile, csvfile,fle: text; pathin: ANSIstring;

function iszero(x, delta:real): boolean;
begin
  iszero:=(x<delta) and (x>-delta);
end;

function samesign(a,b: integer): boolean;
begin
  samesign:=((a<0) and (b<0)) or ((a>=0) and (b>=0))
end;

function fmt(val:integer):str8;
var tstr: str8;
begin
  if val=-1 then fmt:=' +++++++'
  else begin
    str(val:8,tstr);
    fmt:=tstr;
  end;
end;

function ndigit(v:integer): integer;
begin
  if v>=0 then ndigit:=round(log(v))+1 else ndigit:=round(log(v))+2;
end;

{output procedures=============================================================}

procedure writehead;
begin
  writeln;
  write('Structure Life: ',structure_life);
  if gaussian_use_life then write(' +/-',use_life_std:5:1);
  if randomize_period_1_age then writeln(' Initial Period 1 Ages Randomized (Uniformly Distributed)')
  else writeln(' Initial Period 1 Ages Set to 0');
  {writeln('       Start   End   Start Initial   MidPt  Ending   Dated  Target   Early   Early  Growth');
  writeln('Period  Date  Date   Rooms   Dated   Rooms   Rooms   Rooms   Rooms    Prev    Next  Rate %'); }
  writeln('       Start   End   Start Initial   MidPt  Ending   Dated  Target  Growth');
  writeln('Period  Date  Date   Rooms   Dated   Rooms   Rooms   Rooms   Rooms  Rate %');
  if writetxt then begin
    writeln(fle);
    write(fle,'Structure Life: ',structure_life);
    if gaussian_use_life then writeln(fle,' +/-',use_life_std:5:1);
    if randomize_period_1_age then writeln(fle,' Initial Period 1 Ages Randomized (Uniformly Distributed)')
    else writeln(fle,'Initial Period 1 Ages Set to 0');
    writeln(fle,'       Start   End   Start   Midpt  Ending   Dated  Target  Growth');
    writeln(fle,'Period  Date  Date   Rooms   rooms   Rooms   Rooms   Rooms  Rate %');
  end;
end;

procedure writesummary(period: integer);
begin
  write(period:6,period_start[period]:6,period_end[period]:6,
    round(init_rooms[period]/rfactor):8,
    round(initial_dated[period]/rfactor):8,
    round(midpt_rooms[period]/rfactor):8,
    fmt(round(end_rooms[period]/rfactor)),
    fmt(round(dated_rooms[period]/rfactor)),
    round(target_rooms[period]/rfactor):8,
    {round(early_prev[period]/rfactor):8,
    round(early_next[period]/rfactor):8, }
    growth_rate[period]*100:8:3);
  {if randomize_use_life and (min_growth_rate[period]<>1e10) then begin
    writeln(' (',min_growth_rate[period]*100.0:7:3,' - ',max_growth_rate[period]*100.0:7:3,')');
  end
  else} writeln;
  if writecsv then writeln(csvfile,structure_life:3,',',use_life_std:5:1,',',
    round(init_rooms[1]/rfactor):5,',',period,',',
    period_start[period]:5,',',period_end[period]:5,',',
    round(init_rooms[period]/rfactor):5,',',
    round(midpt_rooms[period]/rfactor):5,',',
    fmt(round(end_rooms[period]/rfactor)),',',
    fmt(round(dated_rooms[period]/rfactor)),',',
    round(target_rooms[period]/rfactor):5,',',
    growth_rate[period]*100:8:3);
  if writetxt then begin
    write(fle,period:6,period_start[period]:6,period_end[period]:6,
        round(init_rooms[period]/rfactor):8,
        round(midpt_rooms[period]/rfactor):8,
        fmt(round(end_rooms[period]/rfactor)),
        fmt(round(dated_rooms[period]/rfactor)),
        round(target_rooms[period]/rfactor):8,
        growth_rate[period]*100:8:2);
    {if randomize_use_life  and (min_growth_rate[period]<>1e10) then
      writeln(fle,' (',min_growth_rate[period]*100.0:7:3,' - ',max_growth_rate[period]*100.0:7:3,')')
    else} writeln(fle);
  end;
end;

procedure write_result(lastperiod: integer);
const break=10;
var i,cnt: integer;
Begin
  writehead;
  for i:=1 to lastperiod do writesummary(i);
  {if randomize_use_life then begin
    writeln('Note: The min and max growth rates may not represent the full range');
    if writetxt then writeln(fle,'Note: The min and max growth rates may not represent the full range');
  end;  }
  writeln;
  if writetxt then writeln(fle);
  cnt:=0;
  if screenyear or writetxt or writeyear then begin
    if screenyear then writeln('Occupied Room Counts by Year');
    if writetxt and writeyear then writeln(fle,'Occupied Room Counts by Year');
    for i:=period_start[1] to period_end[lastperiod] do begin
      if (cnt mod break)=0 then begin
        if screenyear then write(i:5,': ');
        if writetxt and writeyear then write(fle,i:5,': ')
      end;
      if screenyear then write(round(rooms[occrm,i]/rfactor):roomdigit,' ');
      if writetxt and writeyear then write(fle,round(rooms[occrm,i]/rfactor):roomdigit,'  ');
      if writeyear then writeln(yearfile,structure_life:3,',',use_life_std:5:1,',',
        round(init_rooms[1]/rfactor):5,',',
        {period,',', period_start[period]:5,',',period_end[period]:5,',',
        round(init_rooms[period]/rfactor):5,',',
        round(midpt_rooms[period]/rfactor):5,',',
        fmt(round(end_rooms[period]/rfactor)),',',
        fmt(round(dated_rooms[period]/rfactor)),',',
        round(target_rooms[period]/rfactor):5,',',
        growth_rate[period]*100:8:3,',',      }
        i:5,',',round(rooms[occrm,i]/rfactor));
      inc(cnt);
      if (cnt mod break)=0 then begin
        if screenyear then writeln;
        if writetxt and writeyear then writeln(fle);
      end;
    end;
  end;
  writeln;
  if writetxt and writeyear then writeln(fle);
end;

procedure writeannualrecord;
var i: integer;
begin
  if readbool('Print Annual Record','F') then begin
    writeln(fle,' Year Occupied','    Built StillOcc');
    for i:=period_start[1] to period_end[period] do
      writeln(fle,i:5,round(rooms[occrm,i]/rfactor):9,
      round(rooms[newrm,i]/rfactor):9,
      round(rooms[abandonThisYear,i]/rfactor):9);
  end;
end;

procedure writeimplied;  {Output the Final Configurations}
var i: integer;
begin
  writeln;
  writeln('Compound Interest Formula Implied Growth Calculation');
  writeln('Period Start   End Implied Growth');
  for i:=1 to nperiod-1 do begin
    write(i:2,' -',i+1:2);
    write((period_start[i]+period_end[i])/2:6:0);
    write((period_start[i+1]+period_end[i+1])/2:6:0);
    writeln((power(1.0*(orig_target_rooms[i+1]/(period_end[i+1]-period_start[i+1]))/
      (orig_target_rooms[i]/(period_end[i]-period_start[i])),
      1/(((period_start[i+1]+period_end[i+1])/2)-
      ((period_start[i]+period_end[i])/2)))-1)*100:15:3);

  end;
  writeln;
  if writetxt then begin
    {writeln(fle,'Note: The min and max growth rates may not represent the full range'); }
    writeln(fle);
    writeln(fle,'Compound Interest Formula Implied Growth Calculation');
    writeln(fle,'Period Start   End Implied Growth');
    for i:=1 to nperiod-1 do
      writeln(fle,i:2,' -',i+1:2,(period_start[i]+period_end[i])/2:6:0,
        (period_start[i+1]+period_end[i+1])/2:6:0,
        (power(1.0*(orig_target_rooms[i+1]/(period_end[i+1]-period_start[i+1]))/
        (orig_target_rooms[i]/(period_end[i]-period_start[i])),
        1/(((period_start[i+1]+period_end[i+1])/2)-
        ((period_start[i]+period_end[i])/2)))-1)*100:15:3);
    writeln(fle);
  end;
end;

procedure closeup;
begin
  if writetxt then close(fle);
  if writecsv then close(csvfile);
  if writeyear then close(yearfile);
  writeln;
  writeln('Program End');
  closeWindow;
end;

procedure unexpected_halt;
begin
   writeln('Unable to Proceed, Program will Halt');
   writeln('Available Results: ');
   write_result(period-1);
   closeup;
   halt;
end;

procedure trace_output (yr: integer);
begin
  if trace>=0 then begin
    inc(lines);
    if ((trace>0) and ((yr mod trace)=0)) or  (yr=period_end[period]) or
      ((target_rooms[period]<=dated_rooms[period]) and not target_warn)
      then
        writeln('Year ',yr,'  Current ',current_rooms,
          '  Abandoned ',abandoned_rooms,'  Dated This Period ',dated_rooms[period],
          '  Dated Next Period ',dated_rooms[period+1]);
    if (target_rooms[period]<=dated_rooms[period]) and (not target_warn) then begin
        writeln('Target Number of Rooms Reached in ',yr-period_start[period],' Years in ',yr);
        target_warn:=true;
    end
    else if (trace<>0) and ((lines mod (trace*24))=0) and
       (not readbool('Continue Trace','T')) then trace:=-1;
  end;
end;


{output procedures=============================================================}

{input procedures=============================================================}

procedure period_data; {Read in Dates and Demographic Data}
var filein: text; i: integer; ok, fail: boolean; str10: string[10];
begin
  end_rooms[0]:=0;
  global_max_rooms:=0;
  pathin:='.TXT';
  readfile('File with Period Data (Reply CON for Keyboard Entry)',filein,pathin);
  if pathin='CON' then begin
    nperiod:=readint('Number of Periods',1,maxperiod,'');
    period_start[1]:=readint('Starting Date for Period 1',earliest+1,latest-structure_life,'');

    for i:=1 to nperiod do begin
      str(i,str10);
      if i>1 then begin
        period_start[i]:=period_end[i-1];
        writeln('Starting Date for Period ',str10,': ',period_start[i]);
      end;
      period_end[i]:=readint('Ending Date for Period '+str10,period_start[i]+1,latest-structure_life,'');
      orig_target_rooms[i]:=readint('Expected Cumulative Rooms for Period '+str10,1,maxrooms,'');
      if orig_target_rooms[i]>global_max_rooms then global_max_rooms:=orig_target_rooms[i];
    end;
  end else begin
    fail:=false;
    writeln('File Must Have: nPeriod, Start[1], End[1], Rooms[1], End[2] Rooms[2]...');
    nperiod:=getinteger(filein);
    ok:=(nperiod>0) and (nperiod<=maxperiod);
    fail:=fail or (not ok);
    if not ok then writeln(nperiod,' - Input Error in Period (1 - maxperiod)');
    period_start[1]:=getinteger(filein);
    ok:=(period_start[1]>=(earliest+1)) and (period_start[1]<=(latest-structure_life));
    fail:=fail or (not ok);
    if not ok then writeln(period_start[1],' - Input Error in Start[1] (earliest+1 - latest-structure_life)');
    for i:=1 to nperiod do begin
      str(i,str10);
      if i>1 then period_start[i]:=period_end[i-1];
      period_end[i]:=getinteger(filein);
      ok:=(period_end[i]>period_start[i]) and (period_end[i]<=(latest-structure_life));
      fail:=fail or (not ok);
      if not ok then writeln(period_end[i],' - Input Error in End['+str10+'] (Start['+str10+'+1 - latest-structure_life)');
      orig_target_rooms[i]:=getinteger(filein);
      ok:=(orig_target_rooms[i]>0) and (orig_target_rooms[i]<=maxrooms);
      if orig_target_rooms[i]>global_max_rooms then global_max_rooms:=orig_target_rooms[i];
      fail:=fail or (not ok);
      if not ok then writeln(orig_target_rooms[i],' - Input Error in Rooms['+str10+'] (1 - ',maxrooms,')');
      roomdigit:=ndigit(global_max_rooms);
    end;

    close(filein);
    if fail then begin
      writeln('Program Ends Due to Input Error');
      closewindow;
      unexpected_halt;
    end;
  end;

  writeln;
end;

procedure output_files;
var tf:boolean;  path,orig_path: ANSIstring;

  function txtoutput: boolean;
  begin
    path:=orig_path+'_T.txt';
    tf:=readbool('Write Output Data to TXT File','F');
    if tf then writefile('TXT Output File',fle,path);
    txtoutput:=tf;
  end;

  function csvoutput: boolean;
   begin
    tf:=readbool('Write Period by Period Output Data to CSV File','F');
    if tf then begin
      path:=orig_path+'_P.csv';
      writefile('CSV Output File',csvfile,path);
      writeln(csvfile,'UseLife,UseSD,InitRm,Period,PerBeg,PerEnd,PerInitRm,PerMidRm,PerEndRm,PerDatedRm,TargRm,Rate');
    end;
    csvoutput:=tf;
  end;

  function yearoutput: boolean;
   begin
    tf:=readbool('Write Annual Output Data to CSV File','F');
    if tf then begin
      path:=orig_path+'_Y.csv';
      writefile('Annual Output File',yearfile,path);
      writeln(yearfile,'UseLife,UseSD,InitRm,Year,OccRm');
    end;
    yearoutput:=tf;
  end;

begin
  if debug then begin
    writetxt:=false;
    writecsv:=false;
    writeyear:=false;
    randomize;
    writeln;
  end
  else begin
    screenyear:=readbool('Display Annual Room Count on Screen','F');
    orig_path:=file_prefix(pathin);
    writetxt:=txtoutput;
    writecsv:=csvoutput;
    writeyear:=yearoutput;
    writeln;
    setrandom;
    writeln('Random Seed: ',randseed);
    if writetxt then writeln(fle,'Random Seed: ',randseed);
    writeln;
  end;
end;

procedure uselife_parameters(var life: integer; var sd: real; var fact: integer; var gaussian,uniform: boolean);
var str10, str10a: string[10]; logmax: integer;
begin
  str(life,str10);
  life:=readint('Structure Use Life',1,maxstructure_life,str10);
  str(life,str10);
  str(sd:6:1,str10a);
  str10a:=trim(str10a);
  sd:=readreal('  S.D of Structure Use Life (reply 0 for Not Randomized)',0.0,life,str10a);
  gaussian:=not iszero(sd,0.00001);

  {readbool('Randomize Use Lives of Houses around '+str10+' Years','T');}
  {If some randomization is done}
  if gaussian then begin
    str(sd:6:1,str10a);
    str10a:=trim(str10a);
    writeln('    Use Lives are Normally Distributed Around ',str10,' with S.D. of ',str10a);
    writeln('    Use Life Distribution Truncated to: Min=1; Max=2*Structure Life');
  end
  else sd:=0.0;

  uniform:=readbool('  Randomly Age Period 1 Structures (Rec. Unless Uselife<<Period 1 Length)','T');
  {if not gaussian, randomized as uniform from 1 to uselife}

  if gaussian or uniform then begin
    {scale up simulation if randomized to reduce sampling error in growth estimates}
    logmax:=i_minimum(5,trunc(log(global_max_rooms)));
    fact:=round(power(10.0,5-logmax));   {note danger of overruning integers if too large}
    str(fact,str10a);
    {if debug then} fact:=readint('  Scale Factor to Improve Estimates',1,100000,str10a)
    {else writeln('  Scale Factor to Improve Estimates: ',fact)};
  end
  else fact:=1;
end;

function iterate_run(var show: boolean; var t: integer): boolean;
var iter: boolean;
begin {no global variables}
  iter:=readchoice('[I]terate Growth Rate or [U]ser Control','IU','I')='I';

  if not debug then begin
    show:=not iter; {always show iterations if User control}
    t:=-1
  end
  else begin
    if iter then show:=readbool('  Show Growth Rate Iterations','T')
    else show:=true;
    if iter and (not show) then t:=-1
    else t:=readint('  Annual Result Log: Years Between Displays (-1=no log; 0=only period end)',-1,latest,'-1');
  end;
  iterate_run:=iter;
end;

function get_initial_rooms(var last_rm0: integer): integer;
var str10: string[10];
begin  {no global variables}
  writeln;
  str(last_rm0,str10);  {default to last run value}
  str10:=trim(str10);
  last_rm0:=readint('Initial Number of Period 1 Rooms',1,maxrooms,str10);
  get_initial_rooms:=last_rm0;
  writeln;
end;

function get_user_growth_rate(dflt: real):real;
var str10: string[10];
begin  {Ask user for growth rate to try}   {no global variables}
   str(dflt*100:6:4,str10);
   str10:=trim(str10);
   write('Period ',period,': ');
   get_user_growth_rate:=readreal('Annual Growth Rate in %',-100,200,str10)/100.0;
end;

{input procedures=============================================================}

procedure build_room(no,yr,per: integer; gaussian_age, uniform_age: boolean);   {Simulate Room Construction}
var i, age: integer;

begin
   {sum:=0.0;}
   for i:=1 to no do begin
     age:=structure_life;
     if gaussian_age then begin
       age:=round(normal*use_life_std+1.0*structure_life);
       if age<1 then age:=1
       else if age>(structure_life*2) then age:=structure_life*2
     end;
     if uniform_age then age:=floor(random*age)+1;
     {sum:=sum+age;}
     inc(rooms[newrm,yr]);
     inc(rooms[abandonThisYear,yr+age]);
     if last_period or ((yr+(age div 2))<=period_end[per]) then
       inc(dated_rooms[per]) else inc(dated_rooms[per+1]);
   end;
   current_rooms:=current_rooms+no;
end;

procedure abandon_rooms(no {rooms} ,yr: integer);   {Simulate Room Abandonment}
begin
   if rooms[abandonThisYear,yr]<no then begin
     writeln('Call to Abandon_Rooms in a Year When None are Available');
     unexpected_halt;
   end;
   rooms[abandonThisYear,yr]:=rooms[abandonThisYear,yr]-no;
   current_rooms:=current_rooms-no;
   abandoned_rooms:=abandoned_rooms+no;
end;

function abandon_next(no,now,last: integer):boolean;
var yr: integer;    {always abandon oldest rooms first}
begin
  yr:=now{earliest_unabandoned};
  while (no>0) and (yr<={now} (last)) do
    if rooms[abandonThisYear,yr]>0 then begin
      if (period>1) and ((yr-period_start[period]) > round(structure_life/2)) then
        {if room is dated to this period but abandoned early then
          if years actually occupied with in the period < half of the actual structure life then it change to date to the previous period}
        if (yr-period_start[period]) < round(structure_life-(yr-now)/2) then
          inc(early_prev[period])  {chenge is it early enough to count}
      else if (period<nperiod) and ((period_end[period]-yr)<round(structure_life/2)) then begin
        {rooms is dated to next period but abandoned in this one, it will be dated to this one not next}
          inc(early_next[period]);
      end;
      abandon_rooms(1,yr);
      dec(no);
    end else inc(yr);
  {earliest_unabandoned:=yr;}
  abandon_next:=(no=0); {we abandoned all requsted}
end;

function no_solution(per: integer): boolean; {check for guaranteed no solution}
begin
  no_solution:=false;

  {for Debugging}
  {if debug and (period=4) then begin
     writeln('Target: ',target_rooms[period],',  Initial Dated',round(initial_dated),',  Current Dated: ',dated_rooms[period]);
     temp:=readbool('OK','T');
  end;}
  if target_rooms[per]<initial_dated[per] then begin
    writeln('Period ',period,': No Solution: Target Rooms (',
      round(target_rooms[per]/rfactor),') < Initial Dated Rooms (',
      round(initial_dated[per]/rfactor),')');
    no_solution:=true;
  end
  else if (init_rooms[per]=0) and (target_rooms[per]>0) then begin
    writeln('Period ',per,': No Solution: Initial Rooms = '{0},
      round(init_rooms[per]/rfactor),' but Target Rooms (',
      round(target_rooms[per]/rfactor),') > 0 ');
    no_solution:=true;
  end;
end;

procedure clean_slate;  {clear period results}
var i, temp: integer;
begin
  temp:=end_rooms[0];
  for i:=0 to nperiod do begin
    init_rooms[i]:=0;
    midpt_rooms[i]:=0;
    end_rooms[i]:=0;
    dated_rooms[i]:=0;
    initial_dated[i]:=0;
    target_rooms[i]:=0;
    early_prev[i]:=0;
    early_next[i]:=0
  end;
  end_rooms[0]:=temp;
end;

procedure initialize_config;
var i, j: integer;
begin
  for j:=period_start[1] to period_end[nperiod]+2*structure_life do  {1=occupied, 2=new, 3=abandon this year}
    for i:=occrm to abandonThisYear do saved_rooms[i,j]:=0;
  for i:=1 to maxperiod do begin
    save_dated_rooms[i]:=0;
    save_initial_dated[i]:=0;
    save_early_prev[i]:=0;
    save_early_next[i]:=0;
  end;
  {earliest_unabandoned:=period_start[1]-1; }
  
  writeln;
end;

procedure save_config;
var i, j: integer;
begin
  for j:=period_start[1] to period_end[nperiod]+2*structure_life do
    for i:=occrm to abandonThisYear do saved_rooms[i,j]:=rooms[i,j];
  for i:=1 to nperiod do begin
    save_dated_rooms[i]:=dated_rooms[i];
    save_initial_dated[i]:=initial_dated[i];
    save_early_prev[i]:=early_prev[i];
    save_early_next[i]:=early_next[i];
  end;
  {save_earliest_unabandoned:=earliest_unabandoned; }

end;

procedure restore_config;
var i, j: integer;
begin
  for j:=period_start[1] to period_end[nperiod]+2*structure_life do
    for i:=occrm to abandonThisYear do rooms[i,j]:=saved_rooms[i,j];
  for i:=1 to nperiod do begin
    dated_rooms[i]:=save_dated_rooms[i];
    initial_dated[i]:=save_initial_dated[i];
    early_prev[i]:=save_early_prev[i];
    early_next[i]:=save_early_next[i];
  end;
  {earliest_unabandoned:=save_earliest_unabandoned; }

end;

begin {Main Program Block}
{Set Things Up}
  clrscr;
  copyright('RoomAccum',version,years,
      'Population Growth/Room Accumulation Simulation');

  structure_life:=25;
  use_life_std:=0.0;
  orig_rms0:=0;
  end_rooms[0]:=orig_rms0;

  period_data;
  output_files;

  writeimplied;

  iterate_rate:=iterate_run(show_iterations, trace);

  {start annual roomcount array at all 0}
  for j:=period_start[1] to period_end[nperiod]+2*structure_life do
    for i:=occrm to abandonThisYear do begin
      saved_rooms[i,j]:=0;
      rooms[i,j]:=0;
    end;
  for i:=1 to maxperiod do save_dated_rooms[i]:=0;


  repeat  {Start loop for a Full Run Again Loop}
    clean_slate; {clear period results}

    initialize_config;
    save_config;

    uselife_parameters(structure_life,use_life_std,rfactor,gaussian_use_life,randomize_period_1_age);
    randomize_use_life:=(gaussian_use_life or randomize_period_1_age);
    period:=0;
    current_rooms:=0;



    for i:=1 to nperiod do target_rooms[i]:=rfactor*orig_target_rooms[i];
    too_many_rooms:=4*global_max_rooms*rfactor;

    repeat {Start Loop through Periods}
      {Period Initialiazation}
      inc(period);
      first_period_run:=true;
      iter:=0;
      last_period:=(period=nperiod);

      growth_rate[period]:=0.0; {run each period first time with growth rate 0}
      min_growth_rate[period]:=1e10;
      max_growth_rate[period]:=-1e10;
      growth_rate_increment:=0.01; {1%}

      room_error:=0;
      save_randseed:=randseed;

      {if run of initial period then read parameter}
      if period=1 then begin
        orig_rms0:=get_initial_rooms(orig_rms0);
        rms0:=rfactor*orig_rms0;
        end_rooms[0]:=rms0;
        if debug or show_iterations then writehead;
      end;

      {Start Iterate Rates through a Period to Find Best Growth Rate}
      repeat {Simulate a Period with a Growth Rate}
        {restore dated rooms and rooms}
        restore_config;
        randseed:=save_randseed;  {to prevent having between-run variation due to differet seed}
        last_error:=room_error;
        inc(iter);
        write(period,'-',iter,#13);

        rms0:=end_rooms[period-1];
        init_rooms[period]:=rms0;
        abandoned_rooms:=0;

        if period=1 then initial_dated[period]:=init_rooms[period] {assumes structure_use < period 1 length}
        else initial_dated[period]:=dated_rooms[period];

        no_go:=no_solution(period);  {chck for no solution}

        if not no_go then begin;
          if not iterate_rate then growth_rate[period]:=get_user_growth_rate(growth_rate[period]);

          if period=1 then begin
            current_rooms:=0;
            build_room(rms0,period_start[period],period,gaussian_use_life,randomize_period_1_age);
            {save_dated_rooms[period]:=rms0; }
          end
          else begin
            current_rooms:=end_rooms[period-1];
            if rms0>current_rooms then begin  {build new rooms to make up the difference}
              build_room(rms0-end_rooms[period-1],period_start[period],period,gaussian_use_life,false);
              writeln('Too Few Starting Rooms in Period ', period);
              unexpected_halt;
            end
            else if rms0<current_rooms then begin {abandon existing rooms early}
              abandon_error:=abandon_next(end_rooms[period-1]-rms0,period_start[period],period_start[period]+1);
              writeln('Too Many Starting Rooms in Period ', period);
              unexpected_halt;
            end;
          end;

          lines:=0; target_warn:=false;
          skip_to_last_year:=false;
          abandon_error:=false;

          for y:=period_start[period]+1 to period_end[period] do begin {Year loop}
            {age and abandon rooms, not yet abandoned, that have reached their use life }

            if not skip_to_last_year then begin {don't do this if you ave drastically overshot}
              abandon_rooms(rooms[abandonThisYear,y],y);

              {check for -100% growth rate, in that case expected rms=0}
              if iszero(1.00+growth_rate[period],0.000001) then rms:=0 else

              {increment rooms by constant growth rate, does this in an integral way}
              rms:=round(rms0*power(1.0+growth_rate[period],y-period_start[period]));
              if rms < too_many_rooms then begin {stop building and abandoning if too many}
                if current_rooms<rms then {Build new rooms}
                  build_room(rms-current_rooms,y,period,gaussian_use_life,false)
                else begin {abandon existing rooms}
                 {abandon_error:=not abandon_next(current_rooms-rms,period_start[period]-structure_life);}
                  abandon_error:=not abandon_next(current_rooms-rms,y,y+2*structure_life);
                  skip_to_last_year:=abandon_error;
                  if abandon_error then begin
                    writeln('Needed number of Rooms Do Not Exist to Abandon');
                    unexpected_halt;
                  end;
                end;
              end
              else skip_to_last_year:=true;
            end;
            rooms[occrm,y]:=current_rooms;
            trace_output(y);     {debug}
          end; {Year loop}

          if skip_to_last_year then begin
             end_rooms[period]:=-rfactor;
             dated_rooms[period]:=-rfactor;
             if abandon_error then room_error:=-1 else room_error:=1; {positive}
          end
          else begin
            end_rooms[period]:=current_rooms;
            room_error:=dated_rooms[period]-target_rooms[period];
          end;

          if (room_error>0) and iszero(1.0+growth_rate[period],0.000001) then begin
            {growth rate is -100% and still too many rooms}
            writeln('Period ',period,': No Solution: Growth -100% & Target Rooms (',
              round(target_rooms[period]/rfactor),') < Dated Rooms (',
              round(dated_rooms[period]/rfactor),')');
            if writetxt then writeln(fle,'Period ',period,': No Solution: Growth -100% & Target Rooms (',
              round(target_rooms[period]/rfactor),') < Dated Rooms (',
              round(dated_rooms[period]/rfactor),')');
            no_go:=true;
          end;

          if randomize_use_life and
            (round(dated_rooms[period]/rfactor)=round(target_rooms[period]/rfactor))
          then begin
            if growth_rate[period]<min_growth_rate[period] then
              min_growth_rate[period]:=growth_rate[period];
            if growth_rate[period]>max_growth_rate[period] then
              max_growth_rate[period]:=growth_rate[period];
          end;

          if show_iterations then writesummary(period);

         {the program uses 1% increments until it goes too far, then .1% in opposite direction, then .01% smaller in opposite direction etc}
          if (room_error<>0) then begin

            if first_period_run then begin
              if room_error>0 then growth_rate_increment:=-growth_rate_increment
            end else
              if not samesign(room_error,last_error) then
                growth_rate_increment:=-growth_rate_increment/10.0;

            growth_rate[period]:=growth_rate[period]+growth_rate_increment;
          end;

          first_period_run:=false;
        end;
      until (iterate_rate and ((room_error=0) or (abs(growth_rate_increment)<epsilon))) or no_go
        or (not iterate_rate and not readbool('Try Period Again','T'));
     {End Iterate Rates through a Period to Find Best Growth Rate}

      midpt_rooms[period]:=rooms[occrm,round((period_start[period]+period_end[period])/2.0)];

      writeln;
      if debug then writesummary(period);
      save_config;

    until last_period or no_go;
    {End Loop through Periods}

    if no_go then write_result(period-1) else write_result(nperiod);

   until not readbool('Run Again','T');
  {Start Run Again Loop}

  {writeannualrecord;}
  closeup;

end.

