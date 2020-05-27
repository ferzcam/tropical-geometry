use Benchmark qw(:all);
use application 'tropical';

sub s_f1{
    my $f1 = toTropicalPolynomial("min(2x+1, x+y, 2y+1, x+z, y+z, 2z+2)", qw(z x y));
    my $H1 = new Hypersurface<Min>(POLYNOMIAL=>$f1);
    print $H1->VERTICES . "\n". "\n";

    
    # print $H1 -> MAXIMAL_POLYTOPES. "\n". "\n";
    # print $H1 -> WEIGHTS
}

<<<<<<< HEAD
# sub s_f2{
#     my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1+z, y+1+z,2z)", qw(z x y));
#     my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2);
#     my $ds = $H->dual_subdivision();
#     my $cells = transpose($ds->MAXIMAL_CELLS) 
#     print $H2->VERTICES . "\n". "\n";
#     print $cells . "\n". "\n";
# }
=======
sub s_f2{
    my $f2 = toTropicalPolynomial("min(2x+3, x+y, 2y+3, x+1+z, y+1+z,2z)", qw(z x y));
    my $H2 = new Hypersurface<Min>(POLYNOMIAL=>$f2);

    print $H2->VERTICES . "\n". "\n";
}
>>>>>>> 8926a126647f80ffc73880c9c4bb2e6b22c855a2
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

sub s_f6{
    my $f6 = toTropicalPolynomial("min(4x+6, 3x+y+4, 2x+2y+3, x+3y+4, 4y+5, 3x+2+z, 2x+y+z, x+2y+1+z, 3y+4+z, 2x+2+2z, x+y+2z, 2y+3+2z, x+3z, y+2+3z, 4z+5)", qw(z x y));
    my $H6 = new Hypersurface<Min>(POLYNOMIAL=>$f6); 
    print $H6->VERTICES . "\n". "\n";
}

sub s_f7{
    my $f7 = toTropicalPolynomial("min(5x+6, 4x+y+2, 3x+2y+4, 2x+3y, x+4y+3, 5y+8, 4x+6+z, 3x+y+4+z, 2x+2y+3+z, x+3y+4+z, 4y+5+z, 3x+2+2z, 2x+y+2z, x+2y+1+2z, 3y+4+2z, 2x+2+3z, x+y+3z, 2y+3+3z, x+4z, y+2+4z, 5z+5)", qw(z x y));
    my $H7 = new Hypersurface<Min>(POLYNOMIAL=>$f7); 
    print $H7->VERTICES . "\n". "\n";
}

sub s_f8{
    my $f8 = toTropicalPolynomial("min(6x+10, 5x+y+8, 4x+2y+6, 3x+3y+6, 2x+4y+4, x+5y+6, 6y+9, 5x+6+z, 4x+y+2+z, 3x+2y+4+z, 2x+3y+z, x+4y+3+z, 5y+8+z, 4x+6+2z, 3x+y+4+2z, 2x+2y+3+2z, x+3y+4+2z, 4y+5+2z, 3x+2+3z, 2x+y+3z, x+2y+1+3z, 3y+4+3z, 2x+2+4z, x+y+4z, 2y+3+4z, x+5z, y+2+5z, 6z+10)", qw(z x y));
    my $H8 = new Hypersurface<Min>(POLYNOMIAL=>$f8); 
    print $H8->VERTICES . "\n". "\n";
}

sub s_f10{
    my $f10 = toTropicalPolynomial("min(3x + 2y -1, 3x + 2z, 3x - 2y + 4z - 1, 3x - 4y + 6z, x + 2y + 2z, x + 4z + 1, 2y + 3z -1, 1 + 5z,  7z - 2y - 1, 2y -2x + 5z)", qw(z x y));
    my $H10 = new Hypersurface<Min>(POLYNOMIAL=>$f10); 
    print $H10->VERTICES . "\n". "\n";
}




sub s_f9{
    my $f9 = toTropicalPolynomial("min(2x+2y, 2y+2z, 2x+2z, 4z)", qw(z x y));
    my $H9 = new Hypersurface<Min>(POLYNOMIAL=>$f9); 
    print $H9->VERTICES . "\n". "\n";
}

sub s_f31{
    my $f31 = toTropicalPolynomial("min(x+y+z+3, x + 2w, y + 2w, 2+z + 2w, -2 + 3w)", qw(w x y z));
    my $H31 = new Hypersurface<Min>(POLYNOMIAL=>$f31); 
    print $H31->VERTICES . "\n". "\n";
}

sub s_f32{
    my $f32 = toTropicalPolynomial("min(x + w, y + w , y + x, 2 x, 1 + 2w)", qw(w x y));
    my $H32 = new Hypersurface<Min>(POLYNOMIAL=>$f32); 
    print $H32->VERTICES . "\n". "\n";
}

sub s_f33{
    my $f33 = toTropicalPolynomial("min(2y+1+w, 2x-1+w, 2z-1+w, 3x-1)", qw(w x y z));
    my $H33 = new Hypersurface<Min>(POLYNOMIAL=>$f33); 
    print $H33->VERTICES . "\n". "\n";
}

sub s_f41{
    my $f41 = toTropicalPolynomial("min(x+w+3+2v, 2x+2w+2, x+y-5+2v, z+3v, 2x+y+w+10, y+z+2v, -3+4v)", qw(v w x y z));
    my $H41 = new Hypersurface<Min>(POLYNOMIAL=>$f41); 
    print $H41->VERTICES . "\n". "\n";
}


sub s_f51{
    my $f51 = toTropicalPolynomial("min(2x  + 1 + 2u , 2y +1 + 2u , 2x + 2y +1, 3y + 2 + u, 3x +2 + u, x + y + 3 + 2u, 2y + x + 3 + u, 1 + 4u )", qw(u x y));
    my $H51 = new Hypersurface<Min>(POLYNOMIAL=>$f51); 
    print $H51->RAYS . "\n".  "\n";
    print $H51->VERTICES . "\n". "\n";
}




# sub s_p1{
#     my $p1 = toTropicalPolynomial("min(2x  + 1 + 2u , 2y +1 + 2u , 2x + 2y +1, 3y + 2 + u, 3x +2 + u, x + y + 3 + 2u, 2y + x + 3 + u, 1 + 4u )", qw(u x y z));
#     my $p1 = new Hypersurface<Min>(POLYNOMIAL=>$p1); 
#     print $p1->VERTICES . "\n". "\n";
# }

 # 1*x^2 + 1*y^2 +  1*x^2*y^2 +  2*y^3 + 2*x^3 + 3*x*y + 3*y^2 * x +   1
#(x^2 + y^2 + (-1)*z^2)*(v^2+w^2+z^2 + (-1))

#(x^2 + y^2 + (-1)*z^2)*(v^2+w^2+z^2 + (-1))

#y^2*w - x^2*w - z^2*w - x^3

#s_f1();
<<<<<<< HEAD
=======

>>>>>>> 8926a126647f80ffc73880c9c4bb2e6b22c855a2
# s_f2();
#s_f3();
# s_f4();
# s_f5();
#s_f6();
#s_f7();
#s_f8();
<<<<<<< HEAD
#s_p1();
#s_f31();
#s_f32();

s_f51();
=======
#s_f9();
#s_f31();
#s_f32();
#s_f33();
#s_f41();
s_f51();
>>>>>>> 8926a126647f80ffc73880c9c4bb2e6b22c855a2
