import csv
from collections import defaultdict
from scipy.stats import pearsonr,spearmanr, kruskal,wilcoxon
from pylab import *


"""Some analysis of the results file generate by evaluate.py"""


def load_results(filename):
    """Loads a file with results and returns the header and the data rows.
    filename -- results file in CSV format.
    returns tuple(header, rows)
    """
    f = open(filename)
    reader = csv.reader(f)
    header = reader.next()
    rows = list(reader)
    f.close()
    return header,rows


def indices(header):
    """Returns a dict that maps row names to indices"""
    return dict((n,i) for i,n in enumerate(header))


def group_results(results, by, out):
    """Groups result rows by the given list of column names.
    results -- results as created by load_results()
    by -- List of colum names to group by.
    out -- output value (e.g. AUC) to be grouped. Column needs to contain
        numerical values.
    returns a dictionary with the groups as keys and lists of aucs
    as values.
    """
    def key(row): return "-".join(row[i] for i in byidxs)

    groups = defaultdict(list)
    header, rows = results

    name2idx = indices(header)
    byidxs = [name2idx[b] for b in by]
    outidx = name2idx[out]

    for row in rows:
        groups[key(row)].append(float(row[outidx]))
    return groups


def filter_results(results, by):
    """Filters results rows by a given list of key value pairs. Multiple key values
    are interpreted as an OR, meaning only one of them has to match to keep the row.
    Call filter_results in succession to achieve an AND filtering.
    results -- results as created by load_results()
    by -- filter values, eg. [("METHOD":"SPEARMAN"), ("METHOD":"PEARSON"), ("NODES","10")]
    returns new, filtered results object
    """
    print "*** FILTER: "+str(by)+" ***"
    header, rows = results
    name2idx = indices(header)
    by = [(name2idx[n],v) for n,v in by]
    new_rows = []
    for row in rows:
        for i,v in by:
            if row[i] == v: new_rows.append(row)
    return header, new_rows


def plot_boxes(results,by,output, ymin=0.5, ymax=1.0):
    """Creates a box plot.
    results -- results as created by load_results()
    by -- list of colum names to group results by, e.g. ['NETWORK', 'METHOD']
          column names are: NETWORK,NODES,EDGES,SAMPLES,METHOD
    output -- output column to group, e.g 'AUC'
    """
    groups = group_results(results, by, output)
    groups = sorted(groups.items(), key = lambda (name,out): mean(out))
    means = [mean(out) for name,out in groups]
    boxes = [out for name,out in groups]
    names = [name for name,out in groups]
    figure()
    bp = boxplot(boxes, notch=0, patch_artist=True)
    setp(bp['boxes'], color='gray')
    setp(bp['whiskers'], color='gray')
    setp(bp['fliers'], marker='None')
    setp(bp['boxes'], facecolor='0.90')
    #title("-".join(by))
    ylabel(output)
    ylim(ymax = ymax, ymin = ymin)
    ax = gca()
    ax.set_xticklabels(names)
    for loc, spine in ax.spines.iteritems(): spine.set_color('none')
    #ax.spines['top'].set_color('none')
    #ax.spines['right'].set_color('none')
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    for label in ax.get_xticklabels(): label.set_rotation(90)
    plot(range(1,len(means)+1),means, marker='.', color='0.7')
    #savefig("boxplot "+"-".join(by)+".svg", transparent=True, bbox_inches='tight')


def plot_lines(results,by,output, ymin=0.5, ymax=1.0):
    """Creates a line plot with error bars.
    results -- results as created by load_results()
    by -- list of colum names to group results by, e.g. ['NETWORK', 'METHOD']
          column names are: NETWORK,NODES,EDGES,SAMPLES,METHOD
    output -- output column to group, e.g 'AUC'
    """
    groups = group_results(results, by, output)
    groups = sorted(groups.items(), key = lambda (name,out): mean(out))
    means = [mean(out) for name,out in groups]
    stds = [std(out) for name,out in groups]
    names = [name for name,out in groups]
    figure()
    ind = range(1,len(means)+1)
    errorbar(ind, means, yerr=stds, marker='o', markersize=10, color='0.9', ecolor='0.5')
    title("-".join(by))
    ylabel(output)
    ylim(ymax = ymax, ymin = ymin)
    xticks(ind, names )
    ax = gca()
    for loc, spine in ax.spines.iteritems(): spine.set_color('none')
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    for label in ax.get_xticklabels(): label.set_rotation(45)
    #savefig("barplot "+"-".join(by)+".svg", transparent=True, bbox_inches='tight')


def plot_lines_nums(results,by,output, ymin=0.0, ymax=1.0):
    """Creates a line plot with error bars over numbers.
    results -- results as created by load_results()
    by -- list of colum names to group results by, e.g. ['NU', 'AUC']
          column must contain numerical data
    output -- output column to group, e.g 'AUC'
    """
    groups = group_results(results, by, output)
    groups = sorted(groups.items(), key = lambda (num,out): float(num))
    means = [mean(out) for num,out in groups]
    stds = [std(out) for num,out in groups]
    nums = [float(num) for num,out in groups]
    figure()
    errorbar(nums, means, yerr=stds, marker='o', markersize=1, color='0.5', ecolor='0.9')
    ylabel(output)
    ylim(ymax = ymax, ymin = ymin)
    ax = gca()
    for loc, spine in ax.spines.iteritems(): spine.set_color('none')
    ax.yaxis.set_ticks_position('left')
    #savefig("barplot "+"-".join(by)+".svg", transparent=True, bbox_inches='tight')


def plot_bars(results,by,output, ymin=0.5, ymax=1.0):
    """Creates a bar plot.
    results -- results as created by load_results()
    by -- list of colum names to group results by, e.g. ['NETWORK', 'METHOD']
          column names are: NETWORK,NODES,EDGES,SAMPLES,METHOD
    output -- output column to group, e.g 'AUC'
    """
    groups = group_results(results, by, output)
    groups = sorted(groups.items(), key = lambda (name,out): mean(out))
    means = [mean(out) for name,out in groups]
    stds = [std(out) for name,out in groups]
    names = [name for name,out in groups]
    figure()
    ind = arange(len(means))
    width = 0.5
    bar(ind, means, width, color='0.7', edgecolor='none', yerr=stds, ecolor='0.9')
    title("-".join(by))
    ylabel(output)
    ylim(ymax = ymax, ymin = ymin)
    xticks(ind+width/2, names )
    ax = gca()
    for loc, spine in ax.spines.iteritems(): spine.set_color('none')
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    for label in ax.get_xticklabels(): label.set_rotation(90)
    #savefig("barplot "+"-".join(by)+".svg", transparent=True, bbox_inches='tight')


def plot_scatter(results,colname1,colname2):
    """Scatter plot of two columns against each other.
    results -- results as created by load_results()
    colname1 -- first colname, eg. "EDGES"
    colname2 -- second colname, eg. "AUC"
    """
    header,rows = results
    name2idx = indices(header)
    col1,col2 = name2idx[colname1],name2idx[colname2]
    data = [(float(row[col1]),float(row[col2])) for row in rows]
    data = zip(*data)
    figure()
    plot(data[0],data[1], '.')
    r = spearmanr(data[0],data[1])[0]
    title("%s vs %s (r=%.2f)"%(colname1,colname2,r))
    xlabel(colname1)
    ylabel(colname2)


def plot_heatmap(results,colname1,colname2,outcol="AUC"):
    """Heatmap plot of two columns and the mean over the output column.
    results -- results as created by load_results()
    colname1 -- first colname, eg. "NODES"
    colname2 -- second colname, eg. "SAMPLES"
    outcol -- output column, eg. "AUC"
    """
    def labels(col):
        def key(v):
            try: return int(v)
            except: return v
        return sorted(list(set(col)), key=key)
    header,rows = results
    name2idx = indices(header)
    col1,col2,colout = name2idx[colname1],name2idx[colname2],name2idx[outcol]
    cols2out  = defaultdict(list)
    for row in rows:
        cols2out[(row[col1],row[col2])].append(float(row[colout]))
    c1,c2 = zip(*tuple(cols2out.keys()))
    xs,ys = labels(c1),labels(c2)
    mat = [[mean(cols2out[(x,y)]) for x in xs] for y in ys ]
    figure()
    imshow(mat, origin='lower', cmap=cm.hot, interpolation='nearest', aspect='equal' )
    ax = gca()
    ax.xaxis.set_ticklabels(['']+xs)
    ax.xaxis.set_ticks(range(-1,len(xs)+1))
    ax.yaxis.set_ticklabels(['']+ys)
    ax.yaxis.set_ticks(range(-1,len(ys)+1))
    for label in ax.get_xticklabels(): label.set_rotation(90)
    for loc, spine in ax.spines.iteritems(): spine.set_color('none')
    ax.xaxis.set_ticks_position('bottom')
    ax.yaxis.set_ticks_position('left')
    titletext = "%s-%s (%s)"%(colname1,colname2,outcol)
    #title(titletext)
    title(outcol)
    xlabel(colname1)
    #ylabel(colname2)
    colorbar()
    #savefig("heatmap "+titletext+".svg", transparent=True, bbox_inches='tight')


def plot_histograms(results,by,output):
    """Creates distribution plots.
    results -- results as created by load_results()
    by -- list of colum names to create plots for, e.g. ['NETWORK', 'METHOD']
    output -- output to bin, e.g 'AUC'
    """
    groups = group_results(results, by, output)
    groups = sorted(groups.items(), key = lambda (name,out): median(out))
    names = [name for name,out in groups]
    for name,out in groups:
        figure()
        hist(out)
        title(name)
        xlabel(output)
        ylabel('frequency')


def print_averages(results,columns):
    """Computes averages for result columns
    results -- results as created by load_results()
    columns -- list of colum names, e.g. ['Q1', 'AUC']
    """
    header, rows = results
    name2idx = indices(header)
    cols = zip(*rows)
    for name in columns:
        values = map(float,cols[name2idx[name]])
        print "%10s %.3f "%(name, mean(values))


def print_correlations(results,columns):
    """Computes correlations between result columns
    results -- results as created by load_results()
    columns -- list of colum names, e.g. ['Q1', 'AUC']
    """
    header, rows = results
    name2idx = indices(header)
    cols = zip(*rows)
    for i,n1 in enumerate(columns):
        for j,n2 in enumerate(columns):
            if j>=i: break
            v1 = map(float,cols[name2idx[n1]])
            v2 = map(float,cols[name2idx[n2]])
            print "%5.2f %10s %10s"%(pearsonr(v1,v2)[0], n1, n2)



def test_kruskal(results,by,output):
    """Non-parametric version of ANOVA. Unpaired"""
    groups = group_results(results, by, output)
    outs = [array(out) for name,out in groups.items()]
    p = kruskal(*outs)[1]
    print ','.join(by)," vs ",output
    print "Kruskal-Wallis p = %3e"%p,
    if p > 0.01:
        print " => No significant difference"
    else:
        print " => Population means are different"


def test_wilcoxon(results,by,output):
    """Non-parametric version of the paired T-test"""
    groups = group_results(results, by, output)
    bfc = len(groups)  #bonferroni correction
    for i,(name1,out1) in enumerate(groups.items()):
        for j,(name2,out2) in enumerate(groups.items()):
            if j>=i: break
            p = wilcoxon(out1,out2)[1]
            sig = "*" if p < 0.01/bfc else " "
            print "%s %s - %s :   %3e"%(sig,name1,name2,p)



def main():
    results = load_results("results.csv")
    print "COLUMNS:", results[0]
    print "#ROWS:", len(results[1])


    #results = filter_results(results, [('METHOD','AlignMean'), ('METHOD','Ngram:10:100'), ('METHOD','Ctrl')])
    #results = filter_results(results, [('METHOD','N-max:8:50')])
    #results = filter_results(results, [('METHOD','A-max:eu:100')])
    #results = filter_results(results, [('METHOD','CTRL')])
    #results = filter_results(results, [('LGT_LEN','20')])
    #results = filter_results(results, [('SEQ_LEN','1000')])
    #results = filter_results(results, [('TREE_SIZE','64')])

    #plot_bars(results, ['METHOD'], 'AUC')
    #plot_bars(results, ['METHOD','NODES','SAMPLES'], 'AUC')
    plot_bars(results, ['LGT_LEN'], 'MCC', ymin=0.0)
    plot_bars(results, ['SEQ_LEN'], 'MCC', ymin=0.0)
    plot_bars(results, ['TREE_SIZE'], 'MCC', ymin=0.0)
    #plot_bars(results, ['NETWORK'], 'ACC')

    #plot_lines(results, ['METHOD'], 'AUC')
    plot_lines(results, ['METHOD'], 'MCC', ymin=0.0)
    #plot_lines(results, ['SEQ_LEN'], 'MCC', ymin=0.0)
    #plot_lines(results, ['TREE_SIZE'], 'MCC', ymin=0.0)
    #plot_lines(results, ['LGT_LEN'], 'MCC', ymin=0.0)
    #plot_lines(results, ['LGT_LEN','METHOD'], 'AUC')

    #plot_lines_nums(results, ['T'], 'AUC', ymin=0.5)


    #plot_boxes(results, ['LGT_LEN'], 'AUC')
    #plot_boxes(results, ['METHOD'], 'AUC')

    #plot_heatmap(results, 'TREE_SIZE', 'METHOD', 'AUC')
    plot_heatmap(results, 'TREE_SIZE', 'METHOD', 'MCC')
    plot_heatmap(results, 'SEQ_LEN', 'METHOD', 'MCC')
    plot_heatmap(results, 'LGT_LEN', 'METHOD', 'MCC')


    #plot_scatter(results, 'NU', 'AUC')

    #plot_histograms(results, ['METHOD'], 'AUC')

    #print_averages(results, ['EDGES','GAINS','LOSSES','TP','TN','FP','FN','MCC','AUC','Q1','Q3','Q5'])
    #print_correlations(results,['EDGES','Q1','AUC'])

    #test_kruskal(results, ['METHOD'], 'Q1')
    #test_wilcoxon(results, ['METHOD'], 'Q1')


if __name__ == "__main__":
    print "running..."
    main()
    print "finished."
    show()

