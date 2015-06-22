// File RGB2G2greyscale_test_data.h
// 
// 
// Created by Matlab function write_data_to_file.m version 0.8a

#define RGB2G(R,G,B) 0.299f*R+0.587f*G+0.114f*B

int in_rows = 2;
int in_cols = 3;
float in[7*3] = {
		0.0f, 0.0f, 0.0f, 
		0.5f, 0.5f, 0.5f, 
		1.0f, 1.0f, 1.0f, 
		1.0f, 0.0f, 0.0f, 
		0.0f, 1.0f, 0.0f, 
		0.0f, 0.0f, 1.0f, 
		0.333f, 0.333f, 0.333f,
		};

float out[7*3] = {
		RGB2G(0.0f, 0.0f, 0.0f), RGB2G(0.0f, 0.0f, 0.0f), RGB2G(0.0f, 0.0f, 0.0f), 
		RGB2G(0.5f, 0.5f, 0.5f), RGB2G(0.5f, 0.5f, 0.5f), RGB2G(0.5f, 0.5f, 0.5f), 
		RGB2G(1.0f, 1.0f, 1.0f), RGB2G(1.0f, 1.0f, 1.0f), RGB2G(1.0f, 1.0f, 1.0f), 
		RGB2G(1.0f, 0.0f, 0.0f), RGB2G(1.0f, 0.0f, 0.0f), RGB2G(1.0f, 0.0f, 0.0f), 
		RGB2G(0.0f, 1.0f, 0.0f), RGB2G(0.0f, 1.0f, 0.0f), RGB2G(0.0f, 1.0f, 0.0f), 
		RGB2G(0.0f, 0.0f, 1.0f), RGB2G(0.0f, 0.0f, 1.0f), RGB2G(0.0f, 0.0f, 1.0f),
		RGB2G(0.333f, 0.333f, 0.333f), RGB2G(0.333f, 0.333f, 0.333f), RGB2G(0.333f, 0.333f, 0.333f)
		};
int out_size = 2*3*3;



