function [] = printHeader(filePointer, colNam)
for i = 1:length(colNam)
    fprintf(filePointer,'%s \t', colNam{i});
end
fprintf(filePointer,'\n');
end

