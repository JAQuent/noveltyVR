function [ ] = judgementTask(subNum, counterBalancing)
%judgementTask
% % % % % % % % % % % % % % % % % % % % % % % % % 
% Deep versus shallow encoding task following Otten et al. 2001 for
% noveltyVR
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk)
% Version: 1.1
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
    
    % Block information ADD explanation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    nBlock = 72;
    nTrial = nBlock*4;
    if counterBalancing == 1 || counterBalancing == 7
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(1), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(2), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 1;
        listNum2 = 2;
    elseif counterBalancing == 2 || counterBalancing == 8
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(1), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(3), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 1;
        listNum2 = 3;
    elseif counterBalancing == 3 || counterBalancing == 9
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(2), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(1), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 2;
        listNum2 = 1;
    elseif counterBalancing == 4 || counterBalancing == 10
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(2), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(3), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 2;
        listNum2 = 3;
    elseif counterBalancing == 5 || counterBalancing == 11
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(3), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(1), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 3;
        listNum2 = 1;
    elseif counterBalancing == 6 || counterBalancing == 12
        [aWords, aLiving, aAlphabetic] = textread(strcat('wordList_', num2str(3), '.txt'),'%s %s %s', 'delimiter',' ');
        [bWords, bLiving, bAlphabetic] = textread(strcat('wordList_', num2str(2), '.txt'),'%s %s %s', 'delimiter',' ');
        listNum1 = 3;
        listNum2 = 2;
    else 
        error('Invalid counter balancing');
    end
    % Concatente both lists
    words = {aWords{1:nBlock} bWords{1:nBlock} bWords{nBlock+1:nBlock*2} aWords{nBlock+1:nBlock*2}};
    living = {aLiving{1:nBlock} bLiving{1:nBlock} bLiving{nBlock+1:nBlock*2} aLiving{nBlock+1:nBlock*2}};
    alphabetic = {aAlphabetic{1:nBlock} bAlphabetic{1:nBlock} bAlphabetic{nBlock+1:nBlock*2} aAlphabetic{nBlock+1:nBlock*2}};
    
    
    % Time variables
    maxWordPreTime = 0.3; % Maximum word presentation time in sec
    fixPreTime     = 0.5; % fixation cross presentation time in sec

    % Relevant key codes
    KbName('UnifyKeyNames');
    space        = KbName('space');
    escape       = KbName('ESCAPE');
    responseKeys = [KbName('LeftControl') KbName('rightControl')];

    % Textures and text
    fixLen              = 20; % Size of fixation cross in pixel
    fixWidth            = 3;
    textSize            = [30 25];
    
    % Output files
    fileNam  = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '.dat'); % name of data file to write to
    mSave    = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '.mat'); % name of another data file to write to (in .mat format)
    mSaveALL = strcat('data/noveltyVR_deepShallowEncodingTask_', num2str(subNum), '_', date, '_', time, '_all.mat'); % name of another data file to write to (in .mat format)
    filePointer = fopen(fileNam,'wt'); % opens ASCII file for writing
 
    % Instruction
    lineLength    = 70;
    messageIntro1 = WrapString('Judgement task \n This task is split into 4 blocks. At the beginning of each block you will get instructions what kind of judgement you’re required to make. During each trial you will be presented with one word at a time. Depending on the instructions at beginning of the block, you will need to make one of the following judgements: \n In the alphabetical task, you need to decide whether the first and the last letter are in alphabetical order (non-alphabetical vs. alphabetical). A word starting with B but ending on A is non- alphabetical. The same is true for a word starting and ending with the same letter. If a word for instance starts with A and ends on Y, then it’s alphabetical. You press the left control key for non-alphabetical words and the right control key for alphabetical words. \n In the animacy task, you need to decide whether the word whether it describes an object something is inanimate, in which case you press the left control key, or whether it describes or belongs to a living organism in other word is animate, in which case you press the right control key. An animate word can be a person, an animal, a plant/fruit or just a part of any of those. Please respond as fast and as accurately as possible.',lineLength);
    livingMessage = WrapString('Please press left control key if the word is inanimate and right control key if animate. \n\nPlease press space to continue.',lineLength);
    alphaMessage = WrapString('Please press left control key if the word is non-alphabetical and right control key if alphabetical. \n\nPlease press space to continue.',lineLength);
    
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
    blockPos    = 0.3;
    slack       = Screen('GetFlipInterval', myScreen)/2; % Getting lack for accurate timing
    HideCursor;

    % Response variables
    RT             = zeros(nTrial, 2) - 99;
    responses      = zeros(nTrial, 2) - 99;
    correctness    = zeros(nTrial, 1) - 99;
    results        = cell(nTrial, 15); 
    
     % Specifiying font settings for trials
    Screen('TextColor', myScreen, [0 0 0]); % Sets to normal font color
    Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
    Screen('TextSize', myScreen, textSize(1)); % Sets size to normal
    
%% Experimental loop
    for trial = 1:nTrial
        if trial == 1
            % Block 1
            block = 1;
            list = listNum1;
            if ismember(counterBalancing, 7:12)
                task = 'living';
                question = 'inanimate/animate';
                blockMessage = livingMessage;
            else
                task = 'alphabetical';
                question = 'non-alphabetical/alphabetical';
                blockMessage = alphaMessage;
            end
            
            % Instructions
            DrawFormattedText(myScreen, messageIntro1, 'center', 'center');
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
            if ismember(counterBalancing, 7:12)
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
            if ismember(counterBalancing, 7:12)
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
            if ismember(counterBalancing, 7:12)
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
        [~, secs, keyCode] = KbCheck; % saves whether a key has been pressed, seconds and the key which has been pressed.
        while keyCode(responseKeys(1)) == 0 && keyCode(responseKeys(2)) == 0 
            [~, secs, keyCode] = KbCheck;
            % Flip screen after 1 sec without a response
            % Slack needed to ensure correct presentation time
            if secs - wordOnset >= maxWordPreTime - slack && notFlipped
                DrawFormattedText(myScreen, question, 'center', rect(4)*legendPos);
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
            responses(trial) = 0; 
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

        % .dat file
        fprintf(filePointer,'%i %i %s %s %i %i %i %s %s %s %s %f %f %f %i %i\n', ...
            subNum, ...
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

        %Save everything in a varibles that is saved at the end.
        results{trial, 1}  = subNum;
        results{trial, 2}  = counterBalancing;
        results{trial, 3}  = date;
        results{trial, 4}  = time;
        results{trial, 5}  = trial;
        results{trial, 6}  = block;
        results{trial, 7}  = list;
        results{trial, 8}  = task;
        results{trial, 9}  = living{trial};
        results{trial, 10} = alphabetic{trial};
        results{trial, 11} = words{trial};
        results{trial, 12} = (wordOnset - fixOnset)*1000;
        results{trial, 13} = (wordOffset - wordOnset)*1000;
        results{trial, 14} = RT(trial);
        results{trial, 15} = responses(trial);
        results{trial, 16} = correctness(trial);
        
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


