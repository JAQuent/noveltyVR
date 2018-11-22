% This script runs ratings for noveltyVR and collects demographics
subNum            = input('Subject number: ');
age               = input('Age in years: ');
VR                = input('Have you used VR before? ');
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
date              = datestr(now,'yyyymmdd');
startTime         = datestr(now,'HHMM');


disp('Start experiment');
KbWait;

noveltyVR_ratingTask('ratingTask/instructions.txt', 'ratingTask/questions.txt', subNum, 'noveltyVR_rating');

% Save demographic file
endTime         = datestr(now,'HHMM');
datafilename    = strcat('data/noveltyVR_demographic_' ,num2str(subNum), '_', date, '_', startTime, '.txt');
datafilepointer = fopen(datafilename,'wt'); % opens ASCII file for writing
fprintf(datafilepointer,'%i %i %s %i %s %s %s\n', ...
    subNum,...
    VR,...
    gender,...
    age,...
    date,...
    startTime,...
    endTime);
fclose('all');