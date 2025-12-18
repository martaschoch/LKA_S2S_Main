**===Main do-file for data preparation+++==============================
**===S2S Databases preparation =========================================
*Author:        Marta Schoch
*Last update:	12/18/25
*----------------------------------------------------------------------
*====================================================================
clear all

*=== Set up =================================================================*
global code "C:\Users\wb553773\GitHub\LKA_S2S_Main\Code\Data Preparation"
global data "C:\Users\wb553773\WBG\Marta Schoch - Analysis\Data"
global output "C:\Users\wb553773\WBG\Marta Schoch - Analysis\Out"
global lfs  $data/LFS
global hies $data/HIES	

*=== Run necessary ado files ===================================================*
run "${code}\\ado\winsor2.ado"
run "${code}\\ado\flagout.ado"
	
*=== Harmonize HIES 2019 and LFS 2016-23=======================================*
do "${code}\\01_clean_hies_2019.do"
do "${code}\\02_clean_microsim_2023.do"
do "${code}\\03_clean_lfs_2016.do"
do "${code}\\04_clean_lfs_2019.do"
do "${code}\\05_clean_lfs_2023.do"
	
