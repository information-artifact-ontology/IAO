# The script used to replace the IRIs of BFO 1.1/RO terms with those in BFO 2.0. Since the replacement using the pattern, 
# be careful for the terms which shared the same pattern, such as
# http://www.obofoundry.org/ro/ro.owl#has_participant
# http://www.obofoundry.org/ro/ro.owl#has_part
# Need to replace "has_participant" first, otherwise will cause problem

# Run command under current folder:
# perl replaceIRIs.pl --inFile1 termsMapping.txt -inFile2 iao-main.owl --outFile iao-main-new.owl
# perl replaceIRIs.pl --inFile1 termsMapping.txt -inFile2 external.owl --outFile external-new.owl
# perl replaceIRIs.pl --inFile1 termsMapping.txt -inFile2 externalByHand.owl --outFile externalByHand-new.owl
# perl replaceIRIs.pl --inFile1 termsMapping.txt -inFile2 externalDerived.owl --outFile externalDerived-new.owl
# perl replaceIRIs.pl --inFile1 termsMapping.txt -inFile2 ontology-metadata.owl --outFile ontology-metadata-new.owl


use strict;
use Getopt::Long;
use IO::File;
use Data::Dumper;

my ($inFile1, $inFile2, $outFile);

&GetOptions('inFile1=s' => \$inFile1,
		'inFile2=s' => \$inFile2,
	    'outFile=s' => \$outFile
	   );

my %idMaps = ();		# (old IRI is the key, and new IRI is the replaced one)

my $fh = IO::File->new("<$inFile1") || die "Cannot read input text file '$inFile1': $!";

while(my $line = <$fh>) {
	my @terms = split(/\s+/, $line);
	$idMaps{$terms[0]} = $terms[1];
}

$fh->close();

#print Dumper(\%idMaps);
#STDERR->print("size ".keys(%idMaps)."\n");

$fh = IO::File->new("<$inFile2") || die "Cannot read input text file '$inFile2': $!";

my $lineNum = 0;
my $file = "";
while(my $line = <$fh>) {
	$file .= $line;
}
$fh->close();

while (my($old_iri, $new_iri) = each(%idMaps)){
	$file =~ s/$old_iri/$new_iri/g;
}

$fh = IO::File->new(">$outFile") || die "Cannot write file '$outFile': $!";
$fh->print($file);
$fh->close();
