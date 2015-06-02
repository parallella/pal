#include <stdio.h>
#include <pal.h>

int main()
{
	p_team_t team;
#define FSIZE 30
	int fsize = FSIZE;
	float fin[FSIZE] = {
		0.0036508301982574043, 0.9871656782479628, 0.8250503087567299,
		0.5539384237927152, 0.1250165345477271, 0.16211558855895292,
		0.21714682861699586, 0.5129063108211831, 0.49386591366957766,
		0.6406534126707863, 0.05765515526792475, 0.052640389817631195,
		0.7231431165805478, 0.9386957271218017, 0.7164230681256379,
		0.5364717478549104, 0.345359429155333, 0.893159395692727,
		0.20667137415343362, 0.6142655872838719, 0.2999276314198509,
		0.05620865101114492, 0.403769439594695, 0.5468852789973263,
		0.030562784205062532, 0.8375312375724177, 0.03404075040838328,
		0.7797161703591056, 0.7584878213763965, 0.26744055775017217
	};
	float fout[FSIZE];
	float flast;

#define USIZE 31
	int usize = USIZE;
	uint32_t uin[USIZE] = {
		411911454, 3395329859, 3959549525, 3554529836, 593237641,
		1474457281, 1015258168, 255899840, 3320334391, 1094989731,
		2654264978, 400300378, 2664262002, 4081967383, 3613257828,
		2838644019, 309310169, 3694246730, 247127462, 3099635597,
		2813739755, 3992409480, 200170913, 3822041437, 1407144545,
		3799611437, 4137319130, 2677498382, 3938443001, 1917335911,
		857741686
	};
	uint32_t uout[USIZE];
	float ulast;

	int sorted = 1;
	int i;

	printf("Running p_sort_f32()\n");
	p_sort_f32(fin, fout, fsize, 1, team);
	printf("Results:\n");
	sorted = 1;
	ulast = fout[0];
	for (i = 0; i < fsize; i++) {
		printf("sort_f32(%2d) = %.5f\n", i, fout[i]);
		if (flast > fout[i])
			sorted = 0;
		flast = fout[i];
	}
	printf("Properly sorted: %s.\n", sorted ? "yes" : "no");

	printf("\n");

	printf("Running p_sort_u32()\n");
	p_sort_u32(uin, uout, usize, 1, team);
	printf("Results:\n");
	sorted = 1;
	ulast = uout[0];
	for (i = 0; i < usize; i++) {
		printf("sort_u32(%2d) = %10u\n", i, uout[i]);
		if (ulast > uout[i])
			sorted = 0;
		ulast = uout[i];
	}
	printf("Properly sorted: %s.\n", sorted ? "yes" : "no");

	return 0;
}
