/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file trial_value_market.cpp
* \ingroup Objects
* \brief The TrialValueMarket class header file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include "util/base/include/util.h"
#include <string>
#include "marketplace/include/trial_value_market.h"
#include "containers/include/iinfo.h"

using namespace std;

//! Constructor
TrialValueMarket::TrialValueMarket( const MarketContainer* aContainer ) :
  Market( aContainer )
{   
    // Initialize to 0.001. Use of previous getSmallNumber() is too small and 
    // takes longer to solve.
    mPrice = 0.001;
}

void TrialValueMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type TrialValueMarket::getType() const {
    return IMarketType::TRIAL_VALUE;
}

void TrialValueMarket::initPrice() {
    // Note zero may be a valid price for a trial value.
    // get the minimum price from the market info
    const string LOWER_BOUND_KEY = "lower-bound-supply-price";
    mMinPrice = mMarketInfo->getDouble( LOWER_BOUND_KEY, 0.0 );
}

void TrialValueMarket::setPrice( const double priceIn ) {
    // Prices for TrialValueMarket are solved explicitly by
    // the solver and should only be set by the model
    // when setting the initial "prices" for the market.
    Market::setPrice( priceIn );
}

void TrialValueMarket::set_price_to_last_if_default( const double lastPrice ) {
   //Market::set_price_to_last_if_default( lastPrice );
    // Only initialize the price from last period's price if the price is set to
    // the default. This prevents overwriting read-in initial prices.
    if( mPrice == 0.001 ){
        mPrice = lastPrice;
    }
    // Note zero may be a valid price for a trial value.
}

void TrialValueMarket::set_price_to_last( const double lastPrice ) {
    Market::set_price_to_last( lastPrice );
}

double TrialValueMarket::getPrice() const {
    return std::max( Market::getPrice(), mMinPrice );
}

/*! \brief Add to the the Market an amount of demand in a method based on the Market's type.
* This is the only method that is different for the trial market type. 
* Here is where the price variable is copied to supply, thereby setting up the solution mechanism
* to solve for the trial value of this quantity.
*
* \author Steve Smith
*
* \param demandIn The new demand to add to the current demand.
* \sa setRawDemand
*/
void TrialValueMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double TrialValueMarket::getDemand() const {
    return Market::getDemand();
}

void TrialValueMarket::nullSupply() {
    // TrialValueMarket does not utilize supply instead
    // it is equal to the price.
}

double TrialValueMarket::getSupply() const {
    return Market::getPrice();
}

double TrialValueMarket::getSolverSupply() const {
    return Market::getPrice();
}

void TrialValueMarket::addToSupply( const double supplyIn ) {
    // TrialValueMarket does not utilize supply instead
    // it is equal to the price thus can not be added to.
    assert( false );
}

bool TrialValueMarket::meetsSpecialSolutionCriteria() const {
    // Trial value markets must be solved in all periods including the base
    // period.
    return false;
}

bool TrialValueMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool TrialValueMarket::shouldSolveNR() const {
    // Allow NR to solve trial value markets, even if they don't
    // otherwise meet the NR criteria.  The edfun.cpp module makes
    // special allowance for this type of market.
    return shouldSolve() && ( abs(Market::getPrice()) > util::getVerySmallNumber() || abs(Market::getDemand()) > util::getVerySmallNumber());
}
