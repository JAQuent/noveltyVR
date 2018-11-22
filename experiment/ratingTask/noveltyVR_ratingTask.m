function [] = noveltyVR_ratingTask(instructionFile, questionsFile, subNum, namePrefix)
% % % % % % % % % % % % % % % % % % % % % % % % % 
% Rating study for noveltyVR
% Author: Alexander Quent (alex.quent at mrc-cbu.cam.ac.uk)
% Version: 1.0
% % % % % % % % % % % % % % % % % % % % % % % % %

%% Explanations
% To run just call function with the respective subject number. 
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
    
    % Input variables
    [questions] = textread(questionsFile, '%100s', 'delimiter', '\t');
    nTrial                 = length(questions);
    
    % Output variables & files
    filePointer = fopen(strcat('data/', namePrefix, '_' , num2str(subNum), '_', date, '_', time,'.dat'),'wt'); % opens ASCII file for writing
    mSave       = strcat('data/', namePrefix, '_', num2str(subNum), '_', date, '_', time, '.mat'); % name of another data file to write to (in .mat format)
    mSaveALL    = strcat('data/', namePrefix, '_', num2str(subNum), '_', date, '_', time, 'all.mat'); % name of another data file to write to (in .mat format)
    
    % Get information about the screen and set general things
    Screen('Preference', 'SuppressAllWarnings',0);
    Screen('Preference', 'SkipSyncTests', 1);
    screens       = Screen('Screens');
    if length(screens) > 1 % Checks for the number of screens
        screenNum        = 1;
    else
        screenNum        = 0;
    end
    rect             = Screen('Rect', screenNum); % Gets the dimension of one screen
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
    messageIntro1 = WrapString(instructions, lineLength);
    endPoints     = {'not true at all', 'absolutely true'};

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
        [position, RT] = slideScale(myScreen, questions{trial}, rect, endPoints, 'device', 'mouse', 'scalaposition', 0.9, 'startposition', 'center', 'displayposition', true, 'aborttime', 200);

        %% Saving data
        fprintf(filePointer,'%i %s %s %i %s %f %f\n', ...
            subNum,...
            date,...
            time,...
            trial,...
            questions{trial},...
            position,...
            RT);
        
        results{trial, 1} = subNum;
        results{trial, 2} = date;
        results{trial, 3} = time;
        results{trial, 4} = trial;
        results{trial, 5} = questions{trial};
        results{trial, 6} = position;
        results{trial, 7} = RT;
        
        %% ITI
        Screen('Flip', myScreen);
        WaitSecs(ITI);
    end          
       
    %% End of experiment
    Screen('TextSize', myScreen, textSize(1)); % Sets size to instruction size
    DrawFormattedText(myScreen, horzcat('End of experiment. Thank you for your participation. \n Please press escape to leave.'), 'center', 'center');
    Screen('Flip', myScreen); 
    [~, ~, keyCode] = KbCheck; 
    while keyCode(escape) == 0 
        [~, ~, keyCode] = KbCheck;
    end

    clearvars images
    fclose('all');
    save(mSave, 'results');
    save(mSaveALL);
    Screen('CloseAll')
catch
    rethrow(lasterror)
    clearvars images
    fclose('all');
    Screen('CloseAll')
end
end