function [ ] = recNRKTask(subNum, oldList)
%judgementTask 
% % % % % % % % % % % % % % % % % % % % % % % % % 
% Recognition new/remember/know task for noveltyVR
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk)
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
    date  = datestr(now,'yyyymmdd');
    time  = datestr(now,'HHMMSS');

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
    % I made the presentation time of the fixation cross a sec shorter to save
    % time.
    
    % Relevant key codes
    KbName('UnifyKeyNames');
    space        = KbName('space');
    escape       = KbName('ESCAPE');
    responseKeys = [KbName('n') KbName('r') KbName('f')];
    % numberKeys need sto be adjusted for the respective layout of the
    % keyboard.

    % Textures and text
    fixLen              = 20; % Size of fixation cross in pixel
    fixWidth            = 3;
    textSize            = [30 25];
    
    % Instruction
    lineLength    = 70;
    messageIntro1 = WrapString('Recognition task \n\n In this task, you will be either presented with an old/studied word, a word that you have seen yesterday when you completed the living/non-living judgement task, or with a new word that was not presented during this task. If you think a word is new press n. If you think you have seen this word yesterday hence an old/studied word, you need to decide whether you remember the word or whether the word is familiar to you. If you know you seen this word because for instance you know what you have thought while reading it, because you know which response you gave, because you know which word preceded or succeeded it or associate any other kind of information with seeing this word yesterday, then you choose remember. If you don’t know anything why you think you have seen this word and it just seems familiar, choose familiar. Note that the sense of familiarity can vary in strength. Please respond as fast and accurately as possible. ',lineLength);

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
    legendPos   = 0.9;
    slack       = Screen('GetFlipInterval', myScreen)/2; % Getting lack for accurate timing
    HideCursor;

    % Output files
    fileNam  = strcat('data/noveltyVR_recNRKTask_', num2str(subNum), '_', date, '_', time, '.dat'); % name of data file to write to
    mSave    = strcat('data/noveltyVR_recNRKTask_', num2str(subNum), '_', date, '_', time, '.mat'); % name of another data file to write to (in .mat format)
    mSaveALL = strcat('data/noveltyVR_recNRKTask_', num2str(subNum), '_', date, '_', time, '_all.mat'); % name of another data file to write to (in .mat format)
    filePointer = fopen(fileNam,'wt'); % opens ASCII file for writing
    
    % Loading word lists
    lists   = [1 2];
    newList = lists(lists ~= oldList);
    [oldWords, oldLiving] = textread(strcat('wordList_', num2str(oldList), '.txt'),'%s %n', 'delimiter',' ');
    [newWords, newLiving] = textread(strcat('wordList_', num2str(newList), '.txt'),'%s %n', 'delimiter',' ');
    oldNew = [ones(1, length(oldWords)) zeros(1, length(newWords))];
    words  = vertcat(oldWords, newWords);
    living = [oldLiving newLiving];
    nTrial = length(words);
    
    % Randomizing order of trials
    shuffle = randsample(nTrial, nTrial);
    words   = words(shuffle);
    living  = living(shuffle);
    oldNew  = oldNew(shuffle);

    % Response variables
    RT             = zeros(nTrial, 2) - 99;
    responses      = zeros(nTrial, 2) - 99;
    correctness    = zeros(nTrial, 1) - 99;
    results        = cell(nTrial, 12); 
    

%% Experimental loop
    for trial = 1:nTrial
        if trial == 1
            % Instruction
            Screen('TextSize', myScreen, textSize(2)); % Sets size to instruction size
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
        DrawFormattedText(myScreen, '(n)ew, (r)emeber or (k)now?', 'center', rect(4)*legendPos);
        wordOnset = Screen('Flip', myScreen, fixOnset + fixPreTime);

        % Recording response
        notFlipped = true;
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, seconds and the key which has been pressed.
        while keyCode(responseKeys(1)) == 0 && keyCode(responseKeys(2)) == 0 && keyCode(responseKeys(3)) == 0
            [~, secs, keyCode] = KbCheck;
            % Flip screen after 1 sec without a response
            % Slack needed to ensure correct presentation time
            if secs - wordOnset >= maxWordPreTime - slack && notFlipped
                DrawFormattedText(myScreen, '(n)ew, (r)emeber or (f)amiliar?', 'center', rect(4)*legendPos);
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
            responses(trial) = 0;
        elseif keyCode(responseKeys(2)) == 1
            responses(trial) = 1;
        elseif keyCode(responseKeys(3)) == 1
            responses(trial) = 2; 
        end

        % Coding accuracy
        if (responses(trial) == 1 || responses(trial) == 2) && oldNew(trial) == 1
            correctness(trial) = 1;
        elseif responses(trial) == 0 && oldNew(trial) == 0
            correctness(trial) = 1;
        else
            correctness(trial) = 0;
        end
        
        %% Saving data
        % Create header on frist trial
        if trial == 1
             colNam = {'subNum', 'oldNew', 'date', 'time', 'trial', 'word', 'living', 'preTimeFix', 'preTimeWord', 'RT', 'resp', 'acc'};
             printHeader(filePointer, colNam)
        end

        % Saving data
        fprintf(filePointer,'%i %i %s %s %i %s %i %f %f %f %i %i\n', ...
            subNum,...
            oldNew(trial),...
            date,...
            time,...
            trial,...
            words{trial},...
            living(trial),...
            (wordOnset - fixOnset)*1000,...
            (wordOffset - wordOnset)*1000,...
            RT(trial),...
            responses(trial),...
            responses(trial));

        %Save everything in a varibles that is saved at the end.
        results{trial, 1}  = subNum;
        results{trial, 2}  = oldNew(trial);
        results{trial, 3}  = date;
        results{trial, 4}  = time;
        results{trial, 5}  = trial;
        results{trial, 6}  = words{trial};
        results{trial, 7}  = living(trial);
        results{trial, 8}  = (wordOnset - fixOnset)*1000;
        results{trial, 9}  = (wordOffset - wordOnset)*1000;
        results{trial, 10} = RT(trial);
        results{trial, 11} = responses(trial);
        results{trial, 12} = responses(trial);
        
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


