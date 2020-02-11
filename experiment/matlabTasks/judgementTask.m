function [ ] = judgementTask(subNum, counterBalancing, group)
%JUDGEMENT is deep versus shallow encoding task following Otten et al. 2001 for
% noveltyVR
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk)
% Version: 1.2

%% Explanations
% This function is for running the encoding task for our noveltyVR
% experiment. The encoding task is a ABBA/BAAB design. There are twelve
% different counterbalancing conditions (0 to 11). From three wordlists
% (each 144 words), 2 are assigned to A and B. The assignment of the tasks
% (living/alphabetical) is also counterbalanced. 

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
    Screen('Preference', 'SkipSyncTests', 0);
    screens       = Screen('Screens');
    if length(screens) > 1 % Checks for the number of screens
        screenNum        = 1;
    else
        screenNum        = 0;
    end
  
    % RGB Colors 
    bgColor    = [255, 255, 255];
    fixColor   = [0, 0, 0];
    
    % Block information 
    nBlock = 72;
    nTrial = nBlock*4;
    % Below the respectives wordList are loaded and assign to A and B. 
    if counterBalancing == 0 || counterBalancing == 6
        [aWords, aLiving, aAlphabetic] = textread('wordList_1.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_2.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 1;
        listNum2 = 2;
    elseif counterBalancing == 1 || counterBalancing == 7
        [aWords, aLiving, aAlphabetic] = textread('wordList_1.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_3.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 1;
        listNum2 = 3;
    elseif counterBalancing == 2 || counterBalancing == 8
        [aWords, aLiving, aAlphabetic] = textread('wordList_2.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_1.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 2;
        listNum2 = 1;
    elseif counterBalancing == 3 || counterBalancing == 9
        [aWords, aLiving, aAlphabetic] = textread('wordList_2.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_3.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 2;
        listNum2 = 3;
    elseif counterBalancing == 4 || counterBalancing == 10
        [aWords, aLiving, aAlphabetic] = textread('wordList_3.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_1.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 3;
        listNum2 = 1;
    elseif counterBalancing == 5 || counterBalancing == 11
        [aWords, aLiving, aAlphabetic] = textread('wordList_3.txt','%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread('wordList_2.txt','%s %s %s', 'delimiter',' ');
        listNum1 = 3;
        listNum2 = 2;
    else 
        error('Invalid counter balancing');
    end
    % Concatente both lists according to ABBA/BAAB design. The assignment of the
    % two encoding tasks (living and alphabetical) to A and B is done
    % further down. 
    words      = {aWords{1:nBlock} bWords{:} aWords{nBlock+1:nBlock*2}};
    living     = {aLiving{1:nBlock} bLiving{:} aLiving{nBlock+1:nBlock*2}};
    alphabetic = {aAlphabetic{1:nBlock} bAlphabetic{:} aAlphabetic{nBlock+1:nBlock*2}};

    % Time variables
    maxWordPreTime     = 0.3; % Max word presentation time in sec
    fixPreTime         = 0.5; % Fixation cross presentation time in sec
    maxReminderPreTime = 4.5; % Max reminder presentation time in sec.
    % After maxReminderPreTime, trial ends and is coded as no response.

    % Relevant key codes
    KbName('UnifyKeyNames');
    space        = KbName('space');
    escape       = KbName('ESCAPE');
    responseKeys = [KbName('LeftControl') KbName('rightControl')];
    
    % Output files
    fileNam  = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '.dat'); % name of data file to write to
    mSave    = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '.mat'); % name of another data file to write to (in .mat format)
    mSaveALL = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '_all.mat'); % name of another data file to write to (in .mat format)
    filePointer = fopen(fileNam,'wt'); % opens ASCII file for writing
 
    % Load instruction and wrap strings
    lineLength    = 70;
    instuct1      = fileread('instructions_judgement1.txt');
    instuct2      = fileread('instructions_judgement2.txt');
    instuct3      = fileread('instructions_judgement3.txt');
    messageIntro1 = WrapString2(instuct1, lineLength, 2);
    messageIntro2 = WrapString2(instuct2, lineLength, 2);
    messageIntro3 = WrapString2(instuct3, lineLength, 2);
    livingMessage = WrapString2('Please press left control key if the word is inanimate and right control key if animate. \n\nPlease press space to continue.',lineLength, 2);
    alphaMessage  = WrapString2('Please press left control key if the word is non-alphabetical and right control key if alphabetical. \n\nPlease press space to continue.',lineLength, 2);
    
    % Opening window and setting preferences
    HideCursor;
    try
        [myScreen, rect] = Screen('OpenWindow', screenNum, bgColor);
    catch
        try
            [myScreen, rect] = Screen('OpenWindow', screenNum, bgColor);
        catch
            try
                [myScreen, rect] = Screen('OpenWindow', screenNum, bgColor);
            catch
                try
                    [myScreen, rect] = Screen('OpenWindow', screenNum, bgColor);
                catch
                    [myScreen, rect] = Screen('OpenWindow', screenNum, bgColor);
                end
            end
        end
    end
    % Gets screen information
    center      = round([rect(3) rect(4)]/2);
    slack       = Screen('GetFlipInterval', myScreen)/2; % Getting lack for accurate timing

    % Variables to specify positions where to display
    legendPos   = 0.9;
    blockPos    = 0.3;
    
    % Response variables
    RT             = zeros(nTrial, 2) - 99;
    responses      = zeros(nTrial, 2) - 99;
    correctness    = zeros(nTrial, 1) - 99;
    results        = cell(nTrial, 17); 
    
    % Specifiying font settings for trials
    fixLen              = 20; % Size of fixation cross in pixel
    fixWidth            = 3;
    textSize            = [30 25];
    Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
    Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
    Screen('TextSize', myScreen, textSize(1)); % Sets size to normal
    
%% Experimental loop
    for trial = 1:nTrial
        if trial == 1
            % Block 1
            block = 1;
            list = listNum1;
            if ismember(counterBalancing, 6:11)
                task = 'living';
                question = 'inanimate/animate';
                blockMessage = livingMessage;
            else
                task = 'alphabetical';
                question = 'non-alphabetical/alphabetical';
                blockMessage = alphaMessage;
            end
            
            % Instructions 1
            DrawFormattedText(myScreen, messageIntro1, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
            
            % Instructions 2
            DrawFormattedText(myScreen, messageIntro2, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
            
            % Instructions 3
            DrawFormattedText(myScreen, messageIntro3, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
            
            % Block information
            DrawFormattedText(myScreen, strcat(num2str(block), '. Block'),'center', rect(4)*blockPos);
            DrawFormattedText(myScreen, blockMessage, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
        elseif trial == nBlock + 1
            % Block 2
            block = 2;
            list = listNum2;
            if ismember(counterBalancing, 6:11)
                task = 'alphabetical';
                question = 'non-alphabetical/alphabetical';
                blockMessage = alphaMessage;
            else
                task = 'living';
                question = 'inanimate/animate';
                blockMessage = livingMessage;
            end
            
            % Block information
            DrawFormattedText(myScreen, strcat(num2str(block), '. Block'),'center', rect(4)*blockPos);
            DrawFormattedText(myScreen, blockMessage, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
        elseif trial == nBlock*2 + 1
            % Block 3
            block = 3;
            list = listNum2;
            if ismember(counterBalancing, 6:11)
                task = 'alphabetical';
                question = 'non-alphabetical/alphabetical';
            else
                task = 'living';
                question = 'inanimate/animate';
                blockMessage = livingMessage;
            end
            
            % Block information
            DrawFormattedText(myScreen, strcat(num2str(block), '. Block'), 'center', rect(4)*blockPos);
            DrawFormattedText(myScreen, blockMessage, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end
        elseif trial == nBlock*3 + 1
            % Block 4
            block = 4;
            list = listNum1;
            if ismember(counterBalancing, 6:11)
                task = 'living';
                question = 'inanimate/animate';
                blockMessage = livingMessage;
            else
                task = 'alphabetical';
                question = 'non-alphabetical/alphabetical';
                blockMessage = alphaMessage;
            end
            
            % Block information
            DrawFormattedText(myScreen, strcat(num2str(block), '. Block'), 'center', rect(4)*blockPos);
            DrawFormattedText(myScreen, blockMessage, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end

        end
        %% Fixation cross
        Screen('DrawLine', myScreen, fixColor, center(1)- fixLen, center(2), center(1)+ fixLen, center(2), fixWidth);
        Screen('DrawLine', myScreen, fixColor, center(1), center(2)- fixLen, center(1), center(2)+ fixLen, fixWidth);
        fixOnset = Screen('Flip', myScreen);
        
        %% Presentation of word
        DrawFormattedText(myScreen, question, 'center', rect(4)*legendPos);
        DrawFormattedText(myScreen, words{trial}, 'center', 'center');
        wordOnset = Screen('Flip', myScreen, fixOnset + fixPreTime);

        % Recording response
        notFlipped = true;
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, 
        % seconds and the key which has been pressed.
        while keyCode(responseKeys(1)) == 0 && keyCode(responseKeys(2)) == 0 
            [~, secs, keyCode] = KbCheck;
            % Flip screen after maxWordPreTime without a response
            % Slack needed to ensure correct presentation time
            if secs - wordOnset >= maxWordPreTime - slack && notFlipped
                % Stop displaying word
                DrawFormattedText(myScreen, question, 'center', rect(4)*legendPos);
                wordOffset    = Screen('Flip', myScreen);
                notFlipped    = false;  
                responseGiven = true;
            end
            if secs - wordOnset >= maxReminderPreTime - slack
                % Max time for reminder presentation and response time
                % window reached. This trial is coded as no response. 
                responseGiven = false;
                break
            end
        end
   
        % Continue to display word if response if given before
        % maxWordPreTime.
        if secs - wordOnset < maxWordPreTime - slack
            DrawFormattedText(myScreen, question, 'center', rect(4)*legendPos);
            wordOffset = Screen('Flip', myScreen, wordOnset + maxWordPreTime);
            responseGiven = true;
        end
        
        % Continue to display word if response if given before
        % maxReminderPreTime.
        if secs - wordOnset < maxReminderPreTime - slack
            Screen('Flip', myScreen, wordOnset + maxReminderPreTime);
            responseGiven = true;
        end 
        
        % Wait for key release if a reponse is given
        if responseGiven
            KbReleaseWait;
        end
        
        % Calculating RT in msec
        if responseGiven
            RT(trial) = (secs - wordOnset)*1000;
        else
            RT(trial) = NaN;
        end

        % Coding responses
        if keyCode(responseKeys(2)) == 1
            responses(trial) = 1;
        elseif keyCode(responseKeys(1)) == 1
            responses(trial) = 0; 
        else
            responses(trial) = NaN;
        end

        % Coding accuracy
        if strcmp(task, 'alphabetical')
            % Alphabetical task
            if responses(trial) == 0 && strcmp(alphabetic{trial},'non-alphabetical')
                correctness(trial) = 1;
            elseif responses(trial) == 1 && strcmp(alphabetic{trial},'alphabetical')
                correctness(trial) = 1;
            else 
                correctness(trial) = 0;
            end
        else
            % Living task
            if responses(trial) == 0 && strcmp(living{trial},'inanimate')
                correctness(trial) = 1;
            elseif responses(trial) == 1 && strcmp(living{trial},'animate')
                correctness(trial) = 1;
            else 
                correctness(trial) = 0;
            end
        end

        %% Saving data
        % Create header on frist trial
        if trial == 1
             colNam = {'subNum', ...
                       'group', ...
                       'counterBalancing', ...
                       'date', ...
                       'time', ...
                       'trial', ...
                       'block', ...
                       'list', ...
                       'task', ...
                       'animacy', ...
                       'alphabetical', ...
                       'word', ...
                       'preTimeFix', ...
                       'preTimeWord', ...
                       'RT', ...
                       'resp', ...
                       'acc'};
             printHeader(filePointer, colNam)
        end

        % Write .dat file
        fprintf(filePointer,'%i %i %i %s %s %i %i %i %s %s %s %s %f %f %f %i %i\n', ...
            subNum, ...
            group, ...
            counterBalancing, ...
            date, ...
            time, ...
            trial, ...
            block, ...
            list, ...
            task, ...
            living{trial},...
            alphabetic{trial},...
            words{trial},...
            (wordOnset - fixOnset)*1000,...
            (wordOffset - wordOnset)*1000,...
            RT(trial),...
            responses(trial),...
            correctness(trial));

        % Save everything in a varibles that is saved at the end.
        results{trial, 1}  = subNum;
        results{trial, 2}  = group;
        results{trial, 3}  = counterBalancing;
        results{trial, 4}  = date;
        results{trial, 5}  = time;
        results{trial, 6}  = trial;
        results{trial, 7}  = block;
        results{trial, 8}  = list;
        results{trial, 9}  = task;
        results{trial, 10}  = living{trial};
        results{trial, 11} = alphabetic{trial};
        results{trial, 12} = words{trial};
        results{trial, 13} = (wordOnset - fixOnset)*1000;
        results{trial, 14} = (wordOffset - wordOnset)*1000;
        results{trial, 15} = RT(trial);
        results{trial, 16} = responses(trial);
        results{trial, 17} = correctness(trial);
        
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
    DrawFormattedText(myScreen, horzcat('The end. \n\n Please press escape.'), 'center', 'center');
    Screen('Flip', myScreen);
    [~, ~, keyCode] = KbCheck; 
    while keyCode(escape) == 0 
        [~, ~, keyCode] = KbCheck;
    end

    Screen('CloseAll')
    ShowCursor;
catch
    ShowCursor;
    fclose('all');
    % Saving .m files and closing files
    save(mSave, 'results');
    save(mSaveALL);
    Screen('CloseAll')
    rethrow(lasterror)
end
end