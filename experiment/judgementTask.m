function [ ] = judgementTask(subNum, wordList)
%judgementTask 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% Judgement (living/non-living) task for noveltyVR
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk
% Version: 1.0
% % % % % % % % % % % % % % % % % % % % % % % % %

%% Explanations

try
%% Setting everuthing up
    % Preliminary stuff
    % Clear Matlab/Octave window:
    clc;

    % check for Opengl compatibility, abort otherwise:
    AssertOpenGL;

    % Reseed randomization
    rand('state', sum(100*clock));

    % General information about subject and session
    date  = str2num(datestr(now,'yyyymmdd'));
    time  = str2num(datestr(now,'HHMMSS'));

    % Get information about the screen and set general things
    Screen('Preference', 'SuppressAllWarnings',0);
    Screen('Preference', 'SkipSyncTests', 1);
    screens       = Screen('Screens');
    if length(screens) > 1
        error('Multi display mode not supported.');
    end
    
    % RGB Colors 
    bgColor    = [255, 255, 255];
    fixColor   = [0, 0, 0];
    
    % Time variables
    maxWordPreTime = 1; % Maximum word presentation time in sec
    fixPreTime     = 2; % fixation cross presentation time in sec
    % The values above for chosen based on Fenker, D.B., Frey, J.U., Schuetze, H., 
    % Heipertz, D., Heinze, H.-J., Düzel, E., 2008. Novel scenes improve recollection 
    % and recall of words. J. Cogn. Neurosci. 20, 1250–1265. https://doi.org/10.1162/jocn.2008.20086
    

    % Relevant key codes
    KbName('UnifyKeyNames');
    space        = KbName('space');
    escape       = KbName('ESCAPE');
    responseKeys = [KbName('l') KbName('n')];
    % numberKeys need sto be adjusted for the respective layout of the
    % keyboard.

    % Textures and text
    fixLen              = 20; % Size of fixation cross in pixel
    fixWidth            = 3;
    textSize            = [30 25];
    
    % Instruction
    lineLength    = 70;
    messageIntro1 = WrapString('Judgement task \n\n add introduction',lineLength);

    % Opening window and setting preferences
    try
        [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
    catch
        try
            [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
        catch
            try
                [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
            catch
                try
                    [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
                catch
                    [myScreen, rect]    = Screen('OpenWindow', 0, bgColor);
                end
            end
        end
    end
    center      = round([rect(3) rect(4)]/2);
    slack       = Screen('GetFlipInterval', myScreen)/2; % Getting lack for accurate timing
    HideCursor;

    % Output files
    fileNam  = strcat('data/judgementTask_',num2str(subNum),'.dat'); % name of data file to write to
    mSave    = strcat('data/judgementTask_',num2str(subNum),'.mat'); % name of another data file to write to (in .mat format)
    mSaveALL = strcat('data/judgementTask_',num2str(subNum),'all.mat'); % name of another data file to write to (in .mat format)
    % Checking for existing result file to prevent accidentally overwriting
    % files from a previous subject/session (except for subject numbers > 0):
    filePointer = fopen(fileNam,'wt'); % opens ASCII file for writing
    % Add line with column names
    
    % Creating trials
    [words, living] = textread(strcat('wordList_', num2str(wordList), '.txt'),'%s %n', 'delimiter',' ');
    nTrial          = length(words);
    
    % Randomizing order of trial
    shuffle = randsample(nTrial, nTrial);
    words   = words(shuffle);
    living  = living(shuffle);

    % Response variables
    RT             = zeros(nTrial, 2) - 99;
    responses      = zeros(nTrial, 2) - 99;
    correctness    = zeros(nTrial, 1) - 99;
    results        = cell(nTrial, 22); 
    

%% Experimental loop
    for trial = 1:nTrial
        if trial == 1
            % Instruction
            Screen('TextSize', myScreen, textSize(2)); % Sets size to instruction size
            % Page 1
            DrawFormattedText(myScreen, messageIntro1, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
            
            % Specifiying font settings for trials
            Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
            Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
            Screen('TextSize', myScreen, textSize(1)); % Sets size to normal
        end
        %% Fixation cross
        Screen('DrawLine', myScreen, fixColor, center(1)- fixLen, center(2), center(1)+ fixLen, center(2), fixWidth);
        Screen('DrawLine', myScreen, fixColor, center(1), center(2)- fixLen, center(1), center(2)+ fixLen, fixWidth);
        fixOnset = Screen('Flip', myScreen);
        
        %% Presentation of word
        DrawFormattedText(myScreen, words{trial}, 'center', 'center');
        wordOnset = Screen('Flip', myScreen, fixOnset + fixPreTime);

        % Recording response
        notFlipped = true;
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, seconds and the key which has been pressed.
        while keyCode(responseKeys(1)) == 0 && keyCode(responseKeys(2)) == 0 
            [~, secs, keyCode] = KbCheck;
            % Flip screen after 1 sec without a response
            % Slack needed to ensure correct presentation time
            if secs - wordOnset >= maxWordPreTime - slack && notFlipped
                wordOffset = Screen('Flip', myScreen);
                notFlipped = false;  
            end
        end
        % Displaying word for remaining time
        if secs - wordOnset < maxWordPreTime
            wordOffset     = Screen('Flip', myScreen, wordOnset + maxWordPreTime);
        end
        KbReleaseWait;
        
        % Calculating variables
        RT(trial)   = (secs - wordOnset)*1000;

        % Coding responses
        if keyCode(responseKeys(1)) == 1
            responses(trial) = 1;
        elseif keyCode(responseKeys(2)) == 1
            responses(trial) = 2; 
        end

        % Coding accuracy
        if responses(trial) == living(trial)
            correctness(trial) = 1;
        else
            correctness(trial) = 0;
        end

        %% Saving data
        fprintf(filePointer,'%i %i %i %i %i %s %i %f %f %f %i %i\n', ...
            subNum,...
            wordList,...
            date,...
            time,...
            trial,...
            words{trial},...
            living(trial),...
            (wordOnset - fixOnset)*1000,...
            (wordOffset - wordOnset)*1000,...
            RT(trial),...
            responses(trial),...
            correctness(trial));

        % Save everything in a varibles that is saved at the end.
        % subNo, date, time, 
%         results{trial, 1}  = subNum;
%         results{trial, 2}  = setNum;
%         results{trial, 3}  = date;
%         results{trial, 4}  = time;
%         results{trial, 5}  = trial;
%         results{trial, 6}  = objectNumber(trial);
%         results{trial, 7}  = encodingLocation(trial);
%         results{trial, 8}  = encodingRank(trial);
%         results{trial, 9}  = foil1Location(trial);
        
    end
    %% End of experiment
    % Saving .m files and closing files
    save(mSave, 'results');
    save(mSaveALL);
    fclose('all');

    % End Screen
    Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
    Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
    Screen('TextSize', myScreen, textSize(2)); % Sets size to instruction size
    DrawFormattedText(myScreen, horzcat('The end.'), 'center', 'center');
    Screen('Flip', myScreen);
    [~, ~, keyCode] = KbCheck; 
    while keyCode(escape) == 0 
        [~, ~, keyCode] = KbCheck;
    end

    Screen('CloseAll')
    ShowCursor;
catch
    rethrow(lasterror)
    ShowCursor;
    fclose('all');
    % Saving .m files and closing files
    save(mSave, 'results');
    save(mSaveALL);
    Screen('CloseAll')
end
end


