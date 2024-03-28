% Wrapper script for psigfit
% 18/01/23
% RStudio inputs a data frame, subject number and fitting attempt number to this function

clear
clc
close all

%current folder is the unreal_psychophysics folder
psy_folder = cd;
% add the psigfinit function folder
% I simply make a folder called functions and unzip the psigfit inside
addpath(genpath('functions')) %genpath adds all subfolders within "functions" folder

%% parameters to define
% define here the path to read output
path_input=[]; %If in same folder as this matlab script then: path_input=[];

%define the folder where output csv is stored.
path_output = 'C:\Users\User\Desktop\GitHub\psych_gady\unreal_psychophysics\output_matlab'; % Important! folder must exist. Important! If want to store in  same folder as this matlab script then: path_output=[];


%DEFINE HERE VALUES of % NO (or yes depending on what you decided above)
%that you want to save
thresh_values=[.25,.5,.75,.95];



%% loading staircases
%define the name of the input csv to intake
input_csv_name = 'matlab_fitting_file.csv';

%loading Answers.csv
stair_tbl=readtable(fullfile(path_input,input_csv_name));

%define here subject name
sub_name = sprintf('%03d',stair_tbl.Subject(1));
attempt = max(stair_tbl.Attempt);
%% Processing staircases
%getting the different staircases used
%cond_names={'Grow','Shrink','Delay','Heavy','Light','Fast','Saturated','Unsaturated','Ripple'};%'Slow',
%cond_names={'Delay','Light','Ripple'};
cond_names=unique(stair_tbl.ConditionName);
% creating collector variables
all_cond_vec={};
all_porp_resp=[];
all_stim_val=[];
all_pr2=[];
figure('Position', [10 10 2000 500]);
doAgain = {};
for cond_num=1:length(cond_names)
    %cond_num=1
    %%  extracting the data for fitting
    y=stair_tbl.QuestionResult(contains(stair_tbl.ConditionName,cond_names{cond_num}));
    x=stair_tbl.log_percent(contains(stair_tbl.ConditionName,cond_names{cond_num}));

    %% Fitting with Psi FIT
    %psigfit
    % Getting # times a stimulus was presented (GC) & the value (GR)
    [GC,GR] = groupcounts_YS(x) ; %YS becasue this is Yoni Stein's groupcount function

    for i=1:length(GR)
        n_no(i)=length(find(y(x==GR(i))==1)); %Change to ==0 /1 depending on the type of answer coding of the staircase
    end
    
    %% fitting w/psisigfit toolbox (see demo 1& 2 for explanations)
    data_psi=[GR,n_no',GC];
    %we have 3 things here: value of stim, number of positive or negative ans,
    %number of presentations

    %setting options
    options             = struct;   % initialize as an empty struct
    options.sigmoidName =  'logistic';   % figure this out better logistic/norm
    options.expType     = 'YesNo';  %type of the function to fit

    %Heavy liftting function
    try
        result = psignifit(data_psi,options); % this contains all the info
    catch
      %msgbox('Something went wrong with the fit, please do this condition again', cond_names{cond_num})
      doAgain = [doAgain, cond_names{cond_num}];
      continue
    end
        

    %% getting values of stimuli that give a given % Yes responses
    % get stimulus level for each of the percentages you want
    
    %Important!
    % at line 50 of the getthresholds_GD function there is an assertion commented out
    %this line stops the threshold calculation if the requested threshold
    %value does not exist in the predicted curve. For example sub_023's fit
    %for 'Fast' condition does not cross the 93% detection threshold, so if we ask
    %for the value of the 95% threshold, matlab throws an error and crashes the rest of the script.
    
    %For several reasons I have chosen to let go of this assertion.
    %1. Our psychophysics are decent approximations at best, we do not need 
    %the same accuracy of measurment as the Zaidel lab.
    %2. The assertion disturbed us in cases where a subject made a 'mistake'
    %early in the staircase, making it seem as if the initial values were
    %less than 100% detectable (all our initial values are for sure 100%
    %detectable. Unlike a classic psychophysics paradigm we don't tell subjects
    %that we are performing a staircase so they are more likely to make such "mistakes".
    %3. the problem is more often on the fringes of the curve so we are not
    %worried about using inappropriate values
    %
    for i=1:length(thresh_values)
        thresh_stim(i)=getThreshold_GD(result,thresh_values(i));
    end
    
    %you want 1.a graph, 2. values, 3.R^2
    %% Getting MCFADDENS pseudo R squared- Using Adam & Helen's script
    p_r2=calc_pseudo_r2(result);
    %visualize the results (see Demo 4)
    % intiating options struct
    plotOptions=struct;
    plotOptions.xLabel= ''; %% Altertion name here X la
    plotOptions.yLabel= 'Porportion Detected';
    subplot(3,3,cond_num);
    [hline,hdata] = plotPsych(result,plotOptions);
    
    % Checking if at least 1 data point is on the slope:
    points_counter = 0;
    for i = 1:length(hdata)
        yValue = hdata(i).YData;
        if yValue < 0.85 && yValue > 0.15
            points_counter = points_counter+1;
        end
    end

    % adding to plot the thresh values you want
    for i=1:length(thresh_stim)
        hold on 
        plot([thresh_stim(i), thresh_stim(i)],[0,thresh_values(i)],'-','Color',[0,0,0])
    end
    hold on
    if p_r2>=0.3 && p_r2<=0.990
        clear title
        %new condition added 9.8.2023
    %After seeing that wonderland subjects had a lot of cases where their
    %JND was 4.6 or above, I have decided to automatically define these
    %cases as needing a repeated staircase. This will make the
    %fix_threshold function in R much more simple as well.
    
    %within these rows we need to check if any of the stim values are 4.6
    %or above. If a value is 4.6 on the 1st or 2nd level, this would mean
    %there is now higher value to place at the 3rd and 4th levels, and we
    %won't see a difference in ratings between the levels.
        if points_counter < 1 || thresh_stim(1)>=4.6 || thresh_stim(2)>=4.6
            title({'';sprintf('\n %s, r^2 = %.2f \n L=%s \n',...
                cond_names{cond_num}, p_r2,print_thresh_values(thresh_stim))},'Color','red');
            %msgbox('~~~~~~Bad Fit please do this condition again~~~~~~', cond_names{cond_num})
            doAgain = [doAgain, cond_names{cond_num}];
        else
            title({'';sprintf('\n %s, r^2 = %.2f  \n L=%s \n',...
                cond_names{cond_num}, p_r2, print_thresh_values(thresh_stim))'';});
        end
        title.VerticalAlignment = 'top';
        ax = gca;
        ax.FontSize = 6;
        axis('square')
        
    elseif p_r2<0.3 || isempty(p_r2) || isnan(p_r2) || p_r2>0.990
        clear title
        %msgbox('~~~~~~Bad Fit please do this condition again~~~~~~', cond_names{cond_num})
        doAgain = [doAgain, cond_names{cond_num}];
        try
            title({'';sprintf('\n %s, r^2 = %.2f \n L=%s \n',...
                cond_names{cond_num}, p_r2,print_thresh_values(thresh_stim))'';},'Color','red');

            title.VerticalAlignment = 'top';
            ax = gca;
            ax.FontSize = 6;
            axis('square')
                
        catch
            warning('There is not enough data to produce a good fit, please do %s again', cond_names{cond_num})
            continue
        end
    end
    %add part to save

    %% Storing output
    %storing in collector variable
    %name of condition
    all_cond_vec=[all_cond_vec;repmat(cond_names(cond_num),length(thresh_values),1)];
    % the desired response porportion (i.e. 80% yes)
    all_porp_resp=[all_porp_resp;thresh_values'];
    %Stimulus level needed for that level
    all_stim_val=[all_stim_val;thresh_stim'];
    %pseudo_r^2
    all_pr2=[all_pr2;ones(size(thresh_values')).*p_r2];
    clear data_psi GC GR n_no options p_r2 result plotOptions thresh_stim x y
end

%%saving table
output_tbl=table(all_cond_vec,all_porp_resp,all_stim_val,all_pr2,...
    'VariableNames',{'Condition','prob','my_thresholds','Mcf_pR'});

%saving table
writetable(output_tbl,fullfile(path_output,sprintf('threshold_values_sub_%s.csv',sub_name)));



%% creating a new jnd.csv to run again:
 if size(doAgain) > 0
     retake_print=table(doAgain',...
         'VariableNames',{'doAgain'});
      writetable(retake_print,fullfile(path_output,sprintf('doAgain_values_sub_%s.csv',sub_name)));
 end

current_folder = cd; %current folder is the psychophysics folder
%results_folder = append(current_folder,'\unreal_05\sub_',sub_name,'\results_sub_',sub_name,'_attempt_',num2str(attempt)); %cd to the current subjects' results folder

%cd (results_folder);
filename=string(sub_name);
s = strcat(filename,'_fits.png');
saveas(gcf,s,'png')
%saveas(figure,s,png)
cd('C:\Users\User\Desktop\GitHub\psych_gady\unreal_psychophysics');