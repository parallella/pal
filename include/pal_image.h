/*generic 2D filter*/
void p_filter(float * restrict A, int na, int ma, float * restrict M, 
	      int nm, int mm, float * restrict B);

/*moving average filter*/
void p_average(float * restrict A, int na, int ma, int nm, float scf, 
	       float * restrict B);

/*median filter*/
void p_median();

/*sobel filter*/
void p_sobel();

/*correlation function*/
void p_corr();

/*auto-correlation function*/
void p_acorr();

/*sum of absolute differences*/
void p_sad();

/*m*/
void p_mad();

/*histogram function*/
void p_hist();

/*histogram function*/
void p_threshold();

