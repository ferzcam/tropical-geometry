use Benchmark qw(:all);
use application 'tropical';


sub s_f1{
    my $f1 = toTropicalPolynomial("min(2x+1, x+y, 2y+1, x+z, y+z, 2z+2)", qw(z x y));
    my $H1 = new Hypersurface<Min>(POLYNOMIAL=>$f1);
    $H1->VERTICES;
}

sub s_f2{
    my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1+z, y+1+z,2z)", qw(z x y));
    my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2); 
    $H2->VERTICES;
}
sub s_f3{
    my $f3 = toTropicalPolynomial("min(3x+3, 2x+y+1, x+2y+1, 3y+3, 2x+1+z, x+y+z, 2y+1+z, x+1+2z, y+1+2z, 3z+3 )", qw(z x y));
    my $H3 = new Hypersurface<Min>(POLYNOMIAL=>$f3); 
    $H3->VERTICES;
}

sub s_f4{
    my $f4 = toTropicalPolynomial("min(3x, 2x+y, x+2y, 3y, 2x+z, x+y+z, 2y+z, x+2z, y+2z, 3z)", qw(z x y));
    my $H4 = new Hypersurface<Min>(POLYNOMIAL=>$f4); 
    $H4->VERTICES;
}

sub s_f5{
    my $f5 = toTropicalPolynomial("min(x-y+2, -y+2+z, -2)", qw(z x y));
    my $H5 = new Hypersurface<Min>(POLYNOMIAL=>$f5); 
    $H5->VERTICES;
}

sub myBenchmark{
  my $t1=Benchmark::timeit(1,"s_f1"); 
  print timestr($t1) . "\n";
  my $t2=Benchmark::timeit(1,"s_f2"); 
  print timestr($t2) . "\n";
  my $t3=Benchmark::timeit(1,"s_f3"); 
  print timestr($t3) . "\n";
  my $t4=Benchmark::timeit(1,"s_f4"); 
  print timestr($t4) . "\n";
  my $t5=Benchmark::timeit(1,"s_f5"); 
  print timestr($t5) . "\n";
}

myBenchmark()