use Benchmark qw(:all);
use application 'tropical';


sub s_f1{
    my $f1 = toTropicalPolynomial("min(2x+1, x+y, 2y+1, x+z, y+z, 2z+2)", qw(z x y));
    my $H1 = new Hypersurface<Min>(POLYNOMIAL=>$f1);
    print $H1->VERTICES . "\n". "\n";
}

sub s_f2{
    my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1+z, y+1+z,2z)", qw(z x y));
    my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2); 
    print $H2->VERTICES . "\n". "\n";
}
sub s_f3{
    my $f3 = toTropicalPolynomial("min(3x+3, 2x+y+1, x+2y+1, 3y+3, 2x+1+z, x+y+z, 2y+1+z, x+1+2z, y+1+2z, 3z+3 )", qw(z x y));
    my $H3 = new Hypersurface<Min>(POLYNOMIAL=>$f3); 
    print $H3->VERTICES . "\n". "\n";
}

sub s_f4{
    my $f4 = toTropicalPolynomial("min(3x, 2x+y, x+2y, 3y, 2x+z, x+y+z, 2y+z, x+2z, y+2z, 3z)", qw(z x y));
    my $H4 = new Hypersurface<Min>(POLYNOMIAL=>$f4); 
    print $H4->VERTICES . "\n". "\n";
}

sub s_f5{
    my $f5 = toTropicalPolynomial("min(x-y+2, -y+2+z, -2)", qw(z x y));
    my $H5 = new Hypersurface<Min>(POLYNOMIAL=>$f5); 
    print $H5->VERTICES . "\n". "\n";
}

s_f1();
s_f2();
s_f3();
s_f4();
s_f5();