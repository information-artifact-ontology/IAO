use strict;
use File::Copy;
# Rewrite the old OBI namespace into new purls. Beware UGLY HACKS below

# Policy here. TBD.

my $idNamespace = "http://purl.obofoundry.org/obo/"; # conservative, but less elegant: http://purl.org/obo/OBI/. Current OBO practice: http://purl.org/obo/OBI#
my $namedclassNamespace = "http://purl.obofoundry.org/obo/";
my $propertyNamespace = "http://purl.obofoundry.org/obo/"; # can't stay at top level, since no OBI differentiation with OBI_
my $individualNamespace = "http://purl.obofoundry.org/obo/";

my $debug = 0;

my $cwd=`pwd`;
chop $cwd;
my $trunk=$cwd."/".$0;
$trunk=~s|/src/tools.*||;
my $branches = "$trunk/src/ontology/branches";
my $external = "$trunk/src/ontology/external";
my $ontology = "$trunk/src/ontology";
my $build = "$trunk/build";

# suck in obi.owl to know which files to process
# open TOP, "<$branches/obi.owl" or die("can't find obi.owl");
# my @lines = <TOP>;
# close TOP;

my $copydir = "$build/newids/"; # write the rewritten files here
my $copyexternal = "$build/external/";

my %used; # to record used numeric ids

# extract the names of the branches
# my @obiParts = grep {/<owl:imports rdf:resource="http:\/\/purl.obofoundry.org\/obo\/obi\/(.*)\.owl"/;$1} @lines;
# my @obiParts = map { s/.*\/([A-Za-z.-]*)\.owl.*/$1/;chomp $_ ; $_} @obiParts;


my %uriReplacements;
my %tagReplacements;

# Compute all rewrites Logic:
#  Read the uri report created by lisp, as that is the only safe way to determine which URIs are for properties,classes, etc.
#  Iterate over them, collecting rewrites, and postponing new numeric id allocation until we know which ids are already used.
#  Finally, allocate new ids for alphanumeric names that need an id.

sub computeReplacements
{ open URILIST, "<$build/uri-report.txt" or die("can't open uri list");
  
  my @todo;
  while (<URILIST>)
  { /(\S+)\s+(\S+)/;
   
    my ($type,$uri) = ($1,$2);
 
    my $rewrite = maybeRewriteURI($type,$uri);
   

if (defined $rewrite)
    { if ($rewrite)
      { $uriReplacements{$uri}=$rewrite; }
      else { push @todo,$uri };
    }
    else {$debug && print "don't bother ",$uri,"\n"}
  }
  close URILIST;
  foreach my $do (@todo)
  {
    my $newid = allocateNewId();
    $uriReplacements{$do} = $idNamespace."IAO_".$newid;
    s/.*[\/#]//;
    $tagReplacements{$do}="IAO_".$newid;
    $debug && print $do,"=>",$uriReplacements{$do},"\n";
    print $do,"=>",$uriReplacements{$do},"\n";

  }
}

# Rewrite one URI Logic:
#  If we already are an OBI_\d+ then rewrite to $idNamespace but normalize to 7 digits with leading 0s and return new.
#  If we are a class and don't currently have an OBI_\d+, return 0. Caller will allocate new id.
#  If we are a property and not an OBI_\d+ then rewrite to $propertyNamespace and return new.
#  If we are an individual and not an OBI_\d+ then rewrite to $individualNamespace and return new.
#  Otherwise leave alone (return undef)

sub maybeRewriteURI
{ my ($type,$uri) = @_;
  $debug && print "in: $type $uri\n";
  if ($type =~ /Class|Property|Individual/)
  {if ($uri =~ /http:\/\/purl\.obofoundry\.org\/obo\/(.*)/)
   { my $localname = $1;
     if ( $localname =~ /IAO_(\d+)/)
     { if (length($1)>7) { return 0 } # too long - allocate new id
       my $newnum = sprintf("%07d",$1);
       if ($used{$newnum}) # collision, allocate new id
       { return 0 }
       else
       { $used{$newnum} = 1;
	 return($idNamespace."IAO_".$newnum)
       }
     }
#     elsif ($localname =~ /CurationStatus|EnumerationClass/)
#     { return $namedclassNamespace.$localname }
     else
     { return(0) }
   }
   else {return undef}
  }
  elsif ($type =~ /Property/)
    {if ($uri =~ /http:\/\/purl\.obofoundry\.org\/obo\/(.*)/)
    { if ($1 =~ /IAO_\d+/)
      { $uri = $idNamespace.$1; }
      else
      { $uri = $propertyNamespace.$1 }
    }
    else
    { return (undef) }
  }
  elsif ($type =~ /Individual/)
    {if ($uri =~ /http:\/\/purl\.obofoundry\.org\/obo\/(.*)/)
    { if ($1 =~ /IAO_\d+/)
      { $uri = $idNamespace.$1; }
      else
      { $uri = $individualNamespace.$1 }
    }
    else
    { return (undef) }
  }
}

# Allocated a new id Logic:
#  Use the next numeric id that hasn't already been used. 
my $count=1;
sub allocateNewId
{ my $test;
  for ($test = sprintf("%07d",$count); $used{$test}; $count++) {$test = sprintf("%07d",$count)}
  $used{$test}=1;
  $test;
}

sub replacenames
{ my $count=1;
  #foreach my $part ( @obiParts) {
      # my $path = "$branches/$part".".owl";
  my $path = "$ontology/IAO.owl";
      open PART, "<$path" or die("Trouble loading $path");
      if (! -e $copydir) { mkdir($copydir,0777) };
      # if (! -e "$copyexternal") { mkdir("$copyexternal",0777) };
      # copy("$branches/obid.owl", "$copydir/obid.owl") or die("Couldn't copy obid.owl");
      # foreach my $file qw(bfo11.owl ro.owl ro_bfo_bridge11.owl protege.owl protege-dc.owl)
      # { copy("$external/$file", "$copyexternal/$file") or die("Couldn't copy $file");}
      my $copypath = $copydir."IAO.owl";
      open PARTCOPY, ">$copypath" or die ("Trouble writing $path");
      my $copy;
      while (<PART>) {
	  s/\s*xmlns=.*/   xmlns="$propertyNamespace"/;
	  s/\s*xmlns:obi=.*/   xmlns:obi="$idNamespace"/;
	  s/\s*xml(ns){0,1}:(.+?)="http:\/\/obi.source.*\/(.*)/   xml$1:$2="$idNamespace\obi\/$3/;
# Fix any text curation status
	  my $pre = "curation_status><CurationStatus rdf:about=\"$propertyNamespace";
	  s/curation_status\s*xml:lang="en"\s*>ready_for_release</$pre\OBI_0000318\"\/></;
	  s/curation_status\s*xml:lang="en"\s*>metadata_incomplete</$pre\OBI_0000320\"\/></;
	  s/curation_status\s*xml:lang="en"\s*>metadata_complete</$pre\OBI_0000319\"\/></;
          s/curation_status\s*xml:lang="en"\s*>uncurated</$pre\OBI_0000328\"\/></;
	  s/curation_status\s*xml:lang="en"\s*>pending_final_vetting</$pre\OBI_0000323\"\/></;
	  s/curation_status\s*xml:lang="en"\s*>definition_incomplete.*?</$pre\OBI_0000291\"\/></;
#
	  s/rdf:(about|id|resource)="(.*?)"/"rdf:".$1."=\"".($uriReplacements{$2}||$2)."\""/ge;
	  s/(<\/{0,1})([A-Za-z0-9_]*)/$1.($tagReplacements{$2}||$2)/ge; 
	  print PARTCOPY $_; 
      }
      close PART;
      close PARTCOPY;
  #}
}

computeReplacements();
replacenames();

$DB::single=1;1;

$debug && print join("\n",keys %uriReplacements);
