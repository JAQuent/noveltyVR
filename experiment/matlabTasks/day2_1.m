% This script runs all tasks for noveltyVR and schemaVR4
% Get demographics
VR                = input('Have you used VR before? ');
subNum            = input('Subject number: ');
age               = input('Age in years: ');
cond              = input('Condition: ');
group             = input('Group: ');
setNum            = input('Set number: ');
disp('Gender:');
disp('0 for female');
disp('1 for male');
disp('2 for non-binary');
gender            = input('Indicate gender: ');
if gender == 0
    gender = 'female';
elseif gender == 1
    gender = 'male';
else
    gender = 'non-binary';
end
date      = datestr(now,'yyyymmdd');
startTime = datestr(now,'HHMM');

date              = datestr(now,'yyyymmdd');
startTime         = datestr(now,'HHMM');

% Run judgement task
if VR == 0
    judgementTask(subNum, cond, group)
end
endTime         = datestr(now,'HHMM');

% Save demographics
datafilename    = strcat('data/demographic_noveltyVR_' ,num2str(subNum), '_', date, '_', startTime, '.txt');
datafilepointer = fopen(datafilename,'wt'); % opens ASCII file for writing
fprintf(datafilepointer,'%i %i %i %i %s %i %s %s %s\n', ...
    subNum, ...
    group, ...
    cond, ...
    VR, ...
    gender, ...
    age, ...
    date, ...
    startTime, ...
    endTime);
fclose('all');