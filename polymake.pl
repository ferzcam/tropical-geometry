use Benchmark qw(:all);
use application 'tropical';


sub s_f1{
    my $f1 = toTropicalPolynomial("min(2x+1, x+y, 2y+1, x+z, y+z, 2z+2)");
    my $H1 = new Hypersurface<Min>(POLYNOMIAL=>$f1);
    $H1->VERTICES;
}

sub s_f2{
    my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1, y+1, z )");
    my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2); 
    $H2->VERTICES;
}
sub s_f3{
    my $f3 = toTropicalPolynomial("min(3x+3, 2x+y+1, x+2y+1, 3y+3, 2x+1, x+y, 2y+1, x+1, y+1, 3z+3 )");
    my $H3 = new Hypersurface<Min>(POLYNOMIAL=>$f3); 
    $H3->VERTICES;
}

sub s_f4{
    my $f4 = toTropicalPolynomial("min(3x+3z, 2x+y, x+2y, 3y+3z, 2x+2z, x+y, 2y+2z, x+z, y+z, z)");
    my $H4 = new Hypersurface<Min>(POLYNOMIAL=>$f4); 
    $H4->VERTICES;
}

sub myBenchmark{
  my $t1=Benchmark::timeit(1,"s_f1"); 
  print timestr($t1) . "\n";
  my $t2=Benchmark::timeit(1,"s_f2"); 
  print timestr($t2) . "\n";;
  my $t3=Benchmark::timeit(1,"s_f3"); 
  print timestr($t3) . "\n";;
  my $t4=Benchmark::timeit(1,"s_f4"); 
  print timestr($t4) . "\n";;
}

myBenchmark()