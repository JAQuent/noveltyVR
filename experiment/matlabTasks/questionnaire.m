function [] = questionnaire(instructionFile, inputFile, subCode, namePrefix)
%QUESTIONNAIRE is a PTB experiment function to collect ratings. 
%
% Explanation of the function:
% In order to use this function a .txt file (instructionFile) with the
% instructions and table (e.g. tab-seperated .txt file) that includes the
% questions that are to be answered along with two (or three anchors) need 
% to be proved. If two anchors are provided they are used for left and right. 
% If three anchors are provided, they are used for left, middle and right. 
% The columns that contain the question and the anchors need to be specified
% in the section below. The input file can contain additional information
% (e.g. question ID). The collected data is concatenated to the input data
% and saved together. After each trial, data is saved to a comma-seperated
% .txt file, which is named according the provided namePrefix 
% ('data/questionnaire'), subCode and data and time of the data collection. 
% This prevent accidental overwriting of data. In addition to the .txt file, 
% data is also save in .mat files. The file ending on _all.mat
% contains all internal variables that are specified within the function,
% the file endong on the data and time contains only the data table. 
% 
% Note this function needs version 1.12 of the slideScale: 
% https://github.com/JAQuent/functions-for-matlab/blob/master/slideScale.m
%
% Version: 1.0
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To modify this for other questionnaires modify the section below.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variables to specify
questCol   = 6; % which col contains questions
% Which col contain anchors (if there only 2 anchors, use anchor3Col =
% NaN;)
anchor1Col = 7; 
anchor2Col = 8;
anchor3Col = 9;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
try
%% Setting everything up
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
    
    % Sets number of anchonrs according to input
    if isnan(anchor3Col)
        anchorNum = 2;
    else
        anchorNum = 3;
    end
    
    % Load input file and create a table
    inputTable = readtable(inputFile);
    inputTable_size = size(inputTable);
    nTrial = inputTable_size(1); % number of questions
    results = num2cell(nan(nTrial, 5)); % Creating cell that saves the results
    
    % Output variables & files
    filePointer = strcat(namePrefix, '_' , num2str(subCode), '_', date, '_', time,'.dat');
    mSave       = strcat(namePrefix, '_', num2str(subCode), '_', date, '_', time, '.mat'); % name of another data file to write to (in .mat format)
    mSaveALL    = strcat(namePrefix, '_', num2str(subCode), '_', date, '_', time, '_all.mat'); % name of another data file to write to (in .mat format)
    
    % Get information about the screen and set general things
    Screen('Preference', 'SuppressAllWarnings',0);
    Screen('Preference', 'SkipSyncTests', 1); %% CHANGE!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    screens       = Screen('Screens');
    if length(screens) > 1 % Checks for the number of screens
        screenNum        = 1;
    else
        screenNum        = 0;
    end
    
    % Relevant key codes
    KbName('UnifyKeyNames');
    space  = KbName('space');
    escape = KbName('ESCAPE');

    % Colors, sizes, instrurctions
    bgColor       = [255 255 255];
    textSize      = [20 20];
    lineLength    = 60;
    ITI           = 0.2;
    instructions  = fileread(instructionFile);
    messageIntro1 = WrapString2(instructions, lineLength, 2);

    % Opening window and setting preferences
    try
        [myScreen, rect]    = Screen('OpenWindow', screenNum, bgColor);
    catch
        try
            [myScreen, rect]    = Screen('OpenWindow', screenNum, bgColor);
        catch
            try
                [myScreen, rect]    = Screen('OpenWindow', screenNum, bgColor);
            catch
                try
                    [myScreen, rect]    = Screen('OpenWindow', screenNum, bgColor);
                catch
                    [myScreen, rect]    = Screen('OpenWindow', screenNum, bgColor);
                end
            end
        end
    end
    
    % Text settings
    Screen('TextFont', myScreen, 'DejaVu'); % Sets normal font
    Screen('TextSize', myScreen, textSize(1)); % Sets size to normal

%% Experimental loop
    for trial = 1:nTrial
        % Exercise and instruction
        if trial == 1
            % Page 1
            Screen('TextSize', myScreen, textSize(1)); % Sets size to instruction size
            DrawFormattedText(myScreen, messageIntro1, 'center', 'center');
            Screen('Flip', myScreen);
            KbReleaseWait;
            [~, ~, keyCode] = KbCheck; 
            while keyCode(space) == 0 
                [~, ~, keyCode] = KbCheck;
            end

            Screen('TextSize', myScreen, textSize(2)); % Sets size to normal
        end 

        %% Trial
        % Parse question and anchors
        question = WrapString2(char(inputTable{trial, questCol}), lineLength, 2);
        if anchorNum == 2
            anchors  = inputTable{trial, [anchor1Col, anchor2Col]};
        else
            anchors  = inputTable{trial, [anchor1Col, anchor2Col, anchor3Col]};
        end
                
        [position, RT] = slideScale(myScreen, question, rect, anchors, 'device', 'mouse', 'scalaLength', 0.8, 'scalaposition', 0.8, 'startposition', 'center');

        %% Saving data
        results{trial, 1} = subCode;
        results{trial, 2} = date;
        results{trial, 3} = time;
        results{trial, 4} = position;
        results{trial, 5} = RT;
        
        % Combine results with table and write to .txt file
        data = [inputTable array2table(results)];
        writetable(data, filePointer); % write to file
        
        %% ITI
        Screen('Flip', myScreen);
        WaitSecs(ITI);
    end          
       
    %% End of experiment
    Screen('TextSize', myScreen, textSize(1)); % Sets size to instruction size
    DrawFormattedText(myScreen, horzcat('End of experiment. Thank you for your participation. \n\n Please press escape to leave.'), 'center', 'center');
    Screen('Flip', myScreen); 
    [~, ~, keyCode] = KbCheck; 
    while keyCode(escape) == 0 
        [~, ~, keyCode] = KbCheck;
    end

    clearvars images
    fclose('all');
    save(mSave, 'data');
    save(mSaveALL);
    Screen('CloseAll')
catch
    clearvars images
    fclose('all');
    Screen('CloseAll')
    rethrow(lasterror)
end
end