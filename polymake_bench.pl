use Benchmark qw(:all);
use application 'tropical';


sub s_f1{
    my $f1 = toTropicalPolynomial("min(2x+1, x+y, 2y+1, x+z, y+z, 2z+2)", qw(z x y));
    my $H1 = new Hypersurface<Min>(POLYNOMIAL=>$f1);
    $H1->VERTICES . "\n". "\n";

    
    # $H1 -> MAXIMAL_POLYTOPES. "\n". "\n";
    # $H1 -> WEIGHTS
}

sub s_f2{
    my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1+z, y+1+z,2z)", qw(z x y));
    my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2);

    $H2->VERTICES . "\n". "\n";
}
sub s_f3{
    my $f3 = toTropicalPolynomial("min(3x+3, 2x+y+1, x+2y+1, 3y+3, 2x+1+z, x+y+z, 2y+1+z, x+1+2z, y+1+2z, 3z+3 )", qw(z x y));
    my $H3 = new Hypersurface<Min>(POLYNOMIAL=>$f3); 
    $H3->VERTICES . "\n". "\n";
}

sub s_f4{
    my $f4 = toTropicalPolynomial("min(3x, 2x+y, x+2y, 3y, 2x+z, x+y+z, 2y+z, x+2z, y+2z, 3z)", qw(z x y));
    my $H4 = new Hypersurface<Min>(POLYNOMIAL=>$f4); 
    $H4->VERTICES . "\n". "\n";
}

sub s_f5{
    my $f5 = toTropicalPolynomial("min(x-y+2, -y+2+z, -2)", qw(z x y));
    my $H5 = new Hypersurface<Min>(POLYNOMIAL=>$f5); 
    $H5->VERTICES . "\n". "\n";
}

sub s_f6{
    my $f6 = toTropicalPolynomial("min(4x+6, 3x+y+4, 2x+2y+3, x+3y+4, 4y+5, 3x+2+z, 2x+y+z, x+2y+1+z, 3y+4+z, 2x+2+2z, x+y+2z, 2y+3+2z, x+3z, y+2+3z, 4z+5)", qw(z x y));
    my $H6 = new Hypersurface<Min>(POLYNOMIAL=>$f6); 
    $H6->VERTICES . "\n". "\n";
}

sub s_f7{
    my $f7 = toTropicalPolynomial("min(5x+6, 4x+y+2, 3x+2y+4, 2x+3y, x+4y+3, 5y+8, 4x+6+z, 3x+y+4+z, 2x+2y+3+z, x+3y+4+z, 4y+5+z, 3x+2+2z, 2x+y+2z, x+2y+1+2z, 3y+4+2z, 2x+2+3z, x+y+3z, 2y+3+3z, x+4z, y+2+4z, 5z+5)", qw(z x y));
    my $H7 = new Hypersurface<Min>(POLYNOMIAL=>$f7); 
    $H7->VERTICES . "\n". "\n";
}

sub s_f8{
    my $f8 = toTropicalPolynomial("min(6x+10, 5x+y+8, 4x+2y+6, 3x+3y+6, 2x+4y+4, x+5y+6, 6y+9, 5x+6+z, 4x+y+2+z, 3x+2y+4+z, 2x+3y+z, x+4y+3+z, 5y+8+z, 4x+6+2z, 3x+y+4+2z, 2x+2y+3+2z, x+3y+4+2z, 4y+5+2z, 3x+2+3z, 2x+y+3z, x+2y+1+3z, 3y+4+3z, 2x+2+4z, x+y+4z, 2y+3+4z, x+5z, y+2+5z, 6z+10)", qw(z x y));
    my $H8 = new Hypersurface<Min>(POLYNOMIAL=>$f8); 
    $H8->VERTICES . "\n". "\n";
}



sub s_f9{
    my $f9 = toTropicalPolynomial("min(2x+2y, 2y+2z, 2x+2z, 4z)", qw(z x y));
    my $H9 = new Hypersurface<Min>(POLYNOMIAL=>$f9); 
    $H9->VERTICES . "\n". "\n";
}

sub s_f31{
    my $f31 = toTropicalPolynomial("min(x+y+z+3, x + 2w, y + 2w, 2+z + 2w, -2 + 3w)", qw(w x y z));
    my $H31 = new Hypersurface<Min>(POLYNOMIAL=>$f31); 
    $H31->VERTICES . "\n". "\n";
}

sub s_f32{
    my $f32 = toTropicalPolynomial("min(x, y, z, 1 + w)", qw(w x y z));
    my $H32 = new Hypersurface<Min>(POLYNOMIAL=>$f32); 
    $H32->VERTICES . "\n". "\n";
}

sub s_f33{
    my $f33 = toTropicalPolynomial("min(2y+1+w, 2x-1+w, 2z-1+w, 3x-1)", qw(w x y z));
    my $H33 = new Hypersurface<Min>(POLYNOMIAL=>$f33); 
    $H33->VERTICES . "\n". "\n";
}

sub s_f41{
    my $f41 = toTropicalPolynomial("min(x+w+3+2v, 2x+2w+2, x+y-5+2v, z+3v, 2x+y+w+10, y+z+2v, -3+4v)", qw(v w x y z));
    my $H41 = new Hypersurface<Min>(POLYNOMIAL=>$f41); 
    $H41->VERTICES . "\n". "\n";
}


sub s_f51{
    my $f51 = toTropicalPolynomial("min(2x -1 +2u , 2x+2v, 2x+2w, 2x+2z,
     2y-1 +2u, 2v+2y,2w+2y,2y+2z,
      2z+ 2u-2,2z+2v-1,2z+2w-1,4z-1)", qw(u v w x y z));
    my $H51 = new Hypersurface<Min>(POLYNOMIAL=>$f51); 
    $H51->VERTICES . "\n". "\n";
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
    my $t6=Benchmark::timeit(1,"s_f6"); 
    print timestr($t6) . "\n";
    my $t7=Benchmark::timeit(1,"s_f7"); 
    print timestr($t7) . "\n";
    my $t8=Benchmark::timeit(1,"s_f8"); 
    print timestr($t8) . "\n";
    my $t9=Benchmark::timeit(1,"s_f9"); 
    print timestr($t1) . "\n";
    my $t31=Benchmark::timeit(1,"s_f31"); 
    print timestr($t31) . "\n";
    my $t32=Benchmark::timeit(1,"s_f32"); 
    print timestr($t32) . "\n";
    my $t33=Benchmark::timeit(1,"s_f33"); 
    print timestr($t33) . "\n";
    my $t41=Benchmark::timeit(1,"s_f41"); 
    print timestr($t41) . "\n";
    my $t51=Benchmark::timeit(1,"s_f51"); 
    print timestr($t51) . "\n";
 
}

myBenchmark()