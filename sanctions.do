import delimited "E:\Economic Sanctions\ties.csv", clear 

destring endyear sender1 institution senderconstraint senderveto sendercinc sendergdp targetconstraint targetveto targetcinc targetgdp senderconstraint_imposition senderveto_imposition sendercinc_imposition sendergdp_imposition targetconstraint_imposition targetveto_imposition targetcinc_imposition targetgdp_imposition aliance_imposition finaloutcome, replace force

gen result = 0 if finaloutcome != .

replace result = 1 if (finaloutcome == 1 | finaloutcome ==2 | finaloutcome == 5 | finaloutcome == 6 | finaloutcome == 7 | finaloutcome == 10)

gen stcinc =sendercinc/targetcinc
gen stcinc_imposition = sendercinc_imposition/sendercinc_imposition

gen sgdp = ln(sendergdp)
gen sgdp_imposition = ln(sendergdp_imposition)

gen tgdp = ln(targetgdp)
gen tgdp_imposition = ln(targetgdp_imposition)

gen sendertarget2 = sendertarget * sendertarget
gen targetsender2 = targetsender * targetsender

gen sendertarget2_imposition = sendertarget_imposition * sendertarget_imposition
gen targetsender2_imposition = targetsender_imposition * targetsender_imposition

heckprobit result senderconstraint targetconstraint institution senderveto targetveto sendertarget2 targetsender2 sgdp tgdp stcinc , select(imposition= senderconstraint_imposition targetconstraint_imposition) cluster(sender1iso)

regress  result senderconstraint_imposition targetconstraint_imposition institution senderveto_imposition sendertarget_imposition targetsender_imposition sendertarget2_imposition targetsender2_imposition sgdp_imposition tgdp_imposition if trade ==0 & threat ==1
matrix intB = e(b)
heckprobit result senderconstraint_imposition targetconstraint_imposition institution senderveto_imposition targetveto_imposition sgdp_imposition tgdp_imposition stcinc_imposition sendertarget_imposition targetsender_imposition sendertarget2_imposition targetsender2_imposition, select(imposition= senderconstraint targetconstraint institution senderveto targetveto sendertarget2 targetsender2 sgdp tgdp stcinc) cluster(sender1iso) from(intB) robust, if trade ==0 & threat ==1

regress  result senderconstraint_imposition targetconstraint_imposition institution senderveto_imposition sendertarget_imposition targetsender_imposition sendertarget2_imposition targetsender2_imposition sgdp_imposition tgdp_imposition if trade ==0 & threat ==1
matrix intB = e(b)

heckprobit result senderconstraint_imposition targetconstraint_imposition institution sendertarget2_imposition targetsender2_imposition, select(imposition= senderconstraint targetconstraint institution  ) cluster(sender1iso) robust, if v44 != 0 & sender1 != 2