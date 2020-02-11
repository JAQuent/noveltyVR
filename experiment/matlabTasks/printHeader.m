function [] = printHeader(filePointer, colNam)
for i = 1:length(colNam)
    fprintf(filePointer,'%s ', colNam{i});
end
fprintf(filePointer,'\n');
end

