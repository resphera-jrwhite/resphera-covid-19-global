#!/usr/bin/perl
use Data::Dumper;
use Getopt::Std;
use Scalar::Util qw/looks_like_number/;
use List::Util qw/shuffle sum min max/;
use POSIX qw/ceil floor/;
use File::Basename;
use warnings;
use strict;
use open ':std', ':encoding(UTF-8)';
use Text::CSV;
# --------------------------------------------------------------------
#  Author: James Robert White, PhD
#  Email: jwhite@respherabio.com
# --------------------------------------------------------------------
#  Create formatted dataset for R vis
# --------------------------------------------------------------------
my $outDir = "../analysis/".basename($0); $outDir =~ s/\.pl$//g;
if (-e $outDir){
  `rm -r $outDir`;
}
`mkdir $outDir`;
# --------------------------------------------------------------------
my $EXT = "../external/COVID-19";
# --------------------------------------------------------------------
# update CSSE covid repo pull ---
print "Remember to perform Hopkins covid repo pull...\n";
# --------------------------------------------------------------------
my $topDir = "$EXT/csse_covid_19_data/csse_covid_19_daily_reports";
my $files  = `ls $topDir | grep "csv"`;
chomp($files);
my @files = split "\n", $files;

my %data  = (); # will hold location, date point to deaths
my %dates = ();
foreach my $f (@files){
  my $prefix    = $f;
  print "$prefix...\n";
  $prefix       =~ s/\.csv//g;

  my @prefix    = split /\-/, $prefix; # 03-23-2020
  my $date      = $prefix[2]."-".$prefix[0]."-".$prefix[1];
  $dates{$date} = 1;

  # parse csv
  my @header    = ();
  my $sep       = ",";
  open IN, "$topDir/$f" or die "Error: cannot find $topDir/$f";
  while(<IN>){
    chomp($_);
    # remove BOM
    $_    =~ s/^\N{U+FEFF}//;
    $_    =~ s/\r//g;
    if (!defined($header[1])){
      $_  =~ s/\//\_/g;
      @header = split ",", $_;
    }else{
      my @A = parse_csv1($_);
      my %line = ();
      if ($#header != $#A){
        print "warning: $f num cols mismatch w/ header\n";
      }
      for my $j (0 .. $#header){
        if ($A[$j] eq "US"){
          $A[$j] = "USA";
        }
        if ($A[$j] eq "Mainland China"){
          $A[$j] = "China";
        }
        if ($A[$j] eq "Iran (Islamic Republic of)"){
          $A[$j] = "Iran";
        }
        if ($A[$j] eq "Korea, South"){
          $A[$j] = "South Korea";
        }
        $A[$j] =~ s/^\ //g;
        $line{$header[$j]} = $A[$j];
      }
      # print Dumper(\%line);
      # next;
      # store date, location, and deaths
      my $state    = $line{"Province_State"};
      my $country  = $line{"Country_Region"};
      my $deaths   = $line{"Deaths"};
      if (!defined($deaths) or $deaths eq ""){
        $deaths    = 0;
      }
      $data{$country}{$date}{"Cumulative.Deaths"} += $deaths;
      if ($country eq "USA" and $state !~ /\,/){
        my $stateKey = "$state, $country";
        $data{$stateKey}{$date}{"Cumulative.Deaths"} += $deaths;
      }
    }
  }
  close IN;
} # end of $files

my $lastdate = "NA";
foreach my $date (sort keys %dates){
  foreach my $key (sort keys %data){
    if (!defined($data{$key}{$date}{"Cumulative.Deaths"})){
      $data{$key}{$date}{"Cumulative.Deaths"} = 0;
    }
    $data{$key}{$date}{"Days.from.50th.Death"}     = "NA";
    $data{$key}{$date}{"Prct.Increase.Death.2Day"} = "NA";
    $data{$key}{$date}{"Deaths.per.Day"}           = "NA";
    $data{$key}{$date}{"Deaths.per.Day.3DayMA"}    = "NA";
    $data{$key}{$date}{"LastInSeries"}             = "no";
    $data{$key}{$date}{"Label"}                    = "NA";
  }
  $lastdate = $date;
}

# fix mistakes in data:
$data{"Italy"}{"2020-03-12"}{"Cumulative.Deaths"}  = 1016;
$data{"Spain"}{"2020-03-12"}{"Cumulative.Deaths"}  = 86;
$data{"France"}{"2020-03-15"}{"Cumulative.Deaths"} = 127;
$data{"France"}{"2020-03-16"}{"Cumulative.Deaths"} = 148;
$data{"France"}{"2020-03-17"}{"Cumulative.Deaths"} = 175;
$data{"France"}{"2020-03-18"}{"Cumulative.Deaths"} = 244;
$data{"France"}{"2020-03-19"}{"Cumulative.Deaths"} = 372;

$data{"South Korea"}{"2020-03-10"}{"Cumulative.Deaths"}       = 54;
$data{"Republic of Korea"}{"2020-03-10"}{"Cumulative.Deaths"} = 0;
$data{"Washington, USA"}{"2020-03-05"}{"Cumulative.Deaths"} = 11;
$data{"Washington, USA"}{"2020-03-06"}{"Cumulative.Deaths"} = 12;
$data{"Washington, USA"}{"2020-03-07"}{"Cumulative.Deaths"} = 16;
$data{"Washington, USA"}{"2020-03-08"}{"Cumulative.Deaths"} = 19;
$data{"Washington, USA"}{"2020-03-09"}{"Cumulative.Deaths"} = 22;

$data{"Virginia, USA"}{"2020-04-08"}{"Cumulative.Deaths"} = 75;

$data{"Japan"}{"2020-03-09"}{"Cumulative.Deaths"} = 7;

$data{"China"}{"2020-02-12"}{"Cumulative.Deaths"} = 1213;
$data{"China"}{"2020-02-21"}{"Cumulative.Deaths"} = 2345;
$data{"China"}{"2020-02-23"}{"Cumulative.Deaths"} = 2510;


# add days from 50
my @sortedDates = sort keys %dates;
foreach my $key (sort keys %data){
  my $daysfrom50 = 0;
  foreach my $i (0 .. $#sortedDates){
    my $date    = $sortedDates[$i];
    if ($data{$key}{$date}{"Cumulative.Deaths"} >= 50){
      $daysfrom50++;
      my $dminus1 = $data{$key}{$sortedDates[($i-1)]}{"Cumulative.Deaths"};
      my $dminus2 = $data{$key}{$sortedDates[($i-2)]}{"Cumulative.Deaths"};
      my $dminus3 = $data{$key}{$sortedDates[($i-3)]}{"Cumulative.Deaths"};

      $data{$key}{$date}{"Days.from.50th.Death"} = $daysfrom50;
      $data{$key}{$date}{"Prct.Increase.Death.2Day"} = 100*((($data{$key}{$date}{"Cumulative.Deaths"}/$dminus1)-1)+(($dminus1/$dminus2)-1))/2;

    }
  }
}

foreach my $key (sort keys %data){
  foreach my $i (3 .. $#sortedDates){
    my $date    = $sortedDates[$i];
    if ($data{$key}{$sortedDates[($i-3)]}{"Cumulative.Deaths"} ne "NA"){
      my $dminus1 = $data{$key}{$sortedDates[($i-1)]}{"Cumulative.Deaths"};
      my $dminus2 = $data{$key}{$sortedDates[($i-2)]}{"Cumulative.Deaths"};
      my $dminus3 = $data{$key}{$sortedDates[($i-3)]}{"Cumulative.Deaths"};

      $data{$key}{$date}{"Deaths.per.Day"}           = $data{$key}{$date}{"Cumulative.Deaths"}-$dminus1;
      $data{$key}{$date}{"Deaths.per.Day.3DayMA"}    = (($data{$key}{$date}{"Cumulative.Deaths"}-$dminus1) + ($dminus1-$dminus2) + ($dminus2-$dminus3))/3;
    }
  }
}


# add last in series info
foreach my $key (sort keys %data){
  $data{$key}{$lastdate}{"LastInSeries"} = "yes";
  $data{$key}{$lastdate}{"Label"}        = $key." (".$data{$key}{$lastdate}{"Cumulative.Deaths"}.")";
}

open OUT, ">$outDir/A00.covid19-long-form.txt" or die "Error: cannot write to $outDir/A00.covid19-long-form.txt\n";
print OUT "Country.Region\tDateFormatted\tCumulative.Deaths\tDays.from.50th.Death\tLastInSeries\tLabel\tPrct.Increase.Death.2Day\tDeaths.per.Day\tDeaths.per.Day.3DayMA\n";
foreach my $key (sort keys %data){
  foreach my $date (sort keys %dates){
    print OUT "$key\t$date\t$data{$key}{$date}{'Cumulative.Deaths'}\t$data{$key}{$date}{'Days.from.50th.Death'}\t$data{$key}{$date}{LastInSeries}\t$data{$key}{$date}{Label}\t$data{$key}{$date}{'Prct.Increase.Death.2Day'}\t$data{$key}{$date}{'Deaths.per.Day'}\t$data{$key}{$date}{'Deaths.per.Day.3DayMA'}\n";
  }
}
close OUT;

# --------------------------------------------------------------------
sub parse_csv1
{
  my $line = shift;
  my $csv = Text::CSV->new( );
  return $csv->parse($line) && $csv->fields( );
}
